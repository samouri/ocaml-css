let _ =
  Js.export_all
    (object%js
      method parse (s:(Js.js_string Js.t)) = Lib.Index.parse (Js.to_string s)
      method print ast = Js.string (Lib.Index.print ast)
      method astPrint ast = Js.string (Lib.Index.astPrint ast)
    end);;