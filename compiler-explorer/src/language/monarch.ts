import { languages } from 'monaco-editor'

const definition: languages.IMonarchLanguage  = {
    ignoreCase: true,

    brackets: [
        { open: '{', close: '}', token: 'delimiter.curly' },
        { open: '[', close: ']', token: 'delimiter.square' },
        { open: '(', close: ')', token: 'delimiter.parenthesis' },
    ],

    keywords: [
        'Auto',
        'AutoReadOnly',
        'BetaOnly',
        'Bool',
        'Conditional',
        'Const',
        'CollapsedOnRef',
        'CollapsedOnBase',
        'Collapsed',
        'CustomEvent',
        'CustomEventName',
        'DebugOnly',
        'Default',
        'Else',
        'ElseIf',
        'EndEvent',
        'EndFunction',
        'EndGroup',
        'EndIf',
        'EndProperty',
        'EndState',
        'EndStruct',
        'EndWhile',
        'Event',
        'Extends',
        'Float',
        'Function',
        'Global',
        'Group',
        'Hidden',
        'If',
        'Import',
        'Int',
        'Mandatory',
        'Native',
        'New',
        'Parent',
        'Property',
        'Return',
        'ScriptName',
        'ScriptEventName',
        'Self',
        'State',
        'String',
        'Struct',
        'StructVarName',
        'Var',
        'While',
        'as',
        'is',
        'true',
        'false'
    ],

    operators: [
        '=', '+', '-', '*', '/', '%', '!', '==', '!=', '>',
        '<', '>=', '<=', '||', '&&', '+=', '-=', '*=', '/=', '%='
    ],

    symbols: /[=><!&|+\-*\/%]+/,

    integer: /\d+/,
    hexinteger: /0[xX][0-9a-fA-F]+/,
    float: /\d+\.\d+/,

    escapes: /\\(?:[tunr\\"])/,

    tokenizer: {
        root: [
			// identifiers and keywords
            [/[a-zA-Z_][a-zA-Z_0-9]*/, {
                cases: {
                    '@keywords': { token: 'keyword' },
                    '@default': 'identifier'
                }
            }],

            [/@symbols/, { cases: { '@operators': 'operators' } }],

            // whitespace
            { include: '@whitespace' },

            // numbers
            [/@hexinteger/, 'number.hex'],
            [/@float/, 'number.float'],
            [/@integer/, 'number'],

            // strings
            [/"([^"\\]|\\.)*$/, 'string.invalid'],
            [/"/, { token: 'string.quote', next: '@string' }],

            // comments
            // single-line comment ('; This is a comment')
            [/;[^\/].*$/, 'comment'],
            // multi-line comment (';/ Can go over multiple lines /;')
            [/;\//, 'comment', '@comment'],
            // documentation comment ('{ very cool docs }')
            [/\{/, 'comment', '@comment']
        ],

        whitespace: [
            [/[ \t\n\r]+/, '']
        ],

        comment: [
            [/\/;/, 'comment', '@pop'],
            [/\}/, 'comment', '@pop'],
            [/./, 'comment.content']
        ],

        string: [
            [/[^\\"]+/, 'string'],
            [/@escapes/, 'string.escape'],
            [/\\./, 'string.escape.invalid'],
            [/"/, { token: 'string.quote', next: '@pop' }]
        ]
    }
};

languages.register({
    id: 'papyrus'
});

languages.setMonarchTokensProvider('papyrus', definition);
