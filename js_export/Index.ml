let _ =
  Js.export_all
    (object%js
      method parse = Lib.Index.parse
      method print = Lib.Index.print
      method astPrint = Lib.Index.astPrint
    end)