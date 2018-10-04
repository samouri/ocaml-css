import { pprint, astPrint } from './lib/css-parser';
import codemirror from './lib/codemirror';
import './lib/code-mirror-modes/javascript';
import './lib/code-mirror-modes/css';

window.pprint = pprint;
window.astPrint = astPrint;
window.CodeMirror = codemirror;