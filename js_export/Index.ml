let parse s = Lib.Index.parse (Js.to_string s);;

let _ =
  Js.export_all
    (object%js
      method pprint s = Js.string (Lib.Index.print (parse s))
      method astPrint s = Js.string (Lib.Index.astPrint (parse s))
    end);;