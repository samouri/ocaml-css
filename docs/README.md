# css-parse-explorer

[css-parse-explorer](https://samouri.github.io/ocaml-css/) is a tool to help individuals explore the AST generated by css-parse as well as to prove the viability of using an OCaml generated library in the browser.

### Development

```bash
cd src
npm install
npm start
```

## Deployment

the current process is a hack. 

1. delete all web files (keep src and this README)
2. run `npm run build` from within the `src` directory which should build the app and `mv` the new web files here.
3. commit and push! [GitHub Pages](https://pages.github.com/) should pick up the changes within seconds.