import { Component, createEffect, onCleanup, onMount } from 'solid-js';
import * as monaco from 'monaco-editor';
import useZoom from '../../hooks/useZoom';
import '../../language/monarch';
import init, { run_parser } from '../../wasm/papyrus_compiler_wasm';

import styles from './CodeEditor.module.css';
import exampleCode from '../../../../extern/MrOctopus/nl_mcm/main/source/nl_mcm.psc?raw'

const CodeEditor: Component = () => {
    let code_element!: HTMLDivElement;
    let output_element!: HTMLDivElement;

    let code_editor: monaco.editor.IStandaloneCodeEditor;
    let output_editor: monaco.editor.IStandaloneCodeEditor;

    const { zoomState } = useZoom();

    const compile = () => {
        const value = code_editor.getValue();

        if (!value) {
            output_editor.setValue("");
            return;
        }

        const result = run_parser(value);

        if (!result) {
            output_editor.setValue("");
        } else {
            output_editor.setValue(result);
        }
    }

    const setupEditors = () => {
        code_editor = monaco.editor.create(code_element, {
            automaticLayout: true,
            mouseWheelZoom: true,
            fontSize: zoomState.fontSize,
            value: exampleCode,
            language: 'papyrus',
            theme: 'vs-dark',
        });

        output_editor = monaco.editor.create(output_element, {
            readOnly: true,
            automaticLayout: true,
            mouseWheelZoom: true,
            fontSize: zoomState.fontSize,
            language: 'text',
            theme: 'vs-dark'
        });

        code_editor.onDidChangeModelContent(() => {
            compile();
        });

        compile();
    };

    onMount(() => {
        init().then(() => {
            setupEditors();
        });
    });

    onCleanup(() => {
        code_editor?.dispose();
        output_editor?.dispose();
    });

    createEffect(() => {
        const fontSize = zoomState.fontSize;

        code_editor?.updateOptions({ fontSize });
        output_editor?.updateOptions({ fontSize });
    });

    return (
        <div>
            <div class={styles.Editor} ref={code_element}></div>
            <div class={styles.Editor} ref={output_element}></div>
        </div>
    );
};

export default CodeEditor;
