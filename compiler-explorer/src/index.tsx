/* @refresh reload */
import { render } from 'solid-js/web';

import App from './components/App';
import './index.css';

render(() => <App />, document.getElementById('root') as HTMLElement);
