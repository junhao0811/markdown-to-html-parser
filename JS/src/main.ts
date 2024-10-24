import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first, switchMap, filter } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$)
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newReducer = compose(reducer)(resetState);
                return newReducer;
            }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = value.HTML;
                }
            }
        });
        
    // Handle Save Button Clicks
    const saveButton = document.getElementById("saveButton");

    if (saveButton) {
        // Create an Observable for save button clicks
        const saveButtonClick$ = fromEvent(saveButton, 'click').pipe(
            // Map the click event to the inner HTML of the preview element
            map(() => document.getElementById("preview")?.innerHTML),
            // Filter out null or undefined HTML content
            filter((htmlContent): htmlContent is string => !!htmlContent),
            // Switch to a fetch Observable to save the HTML content
            switchMap((htmlContent) => fetch("/save", {
                method: "POST",
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ html: htmlContent })
            }))
        );

        // Subscribe to the save button click Observable
        saveButtonClick$.subscribe((response) => {
            if (response.ok) {
                alert("HTML saved successfully!");
            } else {
                alert("Failed to save HTML.");
            }
        });
    }

    // Handle Title Input Changes
    const titleInput = document.getElementById("titleInput");

    if (titleInput) {
        // Create an Observable for title input changes
        const titleInput$ = fromEvent(titleInput, 'input').pipe(
            // Map the input event to the new title value
            map(event => (event.target as HTMLInputElement).value)
        );

        // Subscribe to the title input Observable
        titleInput$.subscribe(newTitle => {
            // Update the document title with the new title value
            document.title = newTitle;
        });
    }
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
