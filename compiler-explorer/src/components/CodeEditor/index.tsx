import { Component, createEffect, onCleanup, onMount } from 'solid-js';
import * as monaco from 'monaco-editor';
import useZoom from '../../hooks/useZoom';
import '../../language/monarch';

import styles from './CodeEditor.module.css';
import exampleCode from '../../../../extern/MrOctopus/nl_mcm/main/source/nl_mcm.psc?raw'

const CodeEditor: Component = () => {
    let parent!: HTMLDivElement;
    let editor: monaco.editor.IStandaloneCodeEditor;

    const { zoomState } = useZoom();

    const setupEditor = () => {
        editor = monaco.editor.create(parent, {
            automaticLayout: true,
            mouseWheelZoom: true,
            fontSize: zoomState.fontSize,
            value: exampleCode,
            language: 'papyrus',
            theme: 'vs-dark'
        });
    };

    onMount(() => setupEditor());
    onCleanup(() => editor?.dispose());

    createEffect(() => {
        const fontSize = zoomState.fontSize;
        editor.updateOptions({ fontSize });
    })

    return (
        <div class={styles.Editor} ref={parent}></div>
    );
};

export default CodeEditor;
