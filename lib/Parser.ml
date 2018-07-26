
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | URI of (
# 8 "./lib/Parser.mly"
       (string)
# 11 "./lib/Parser.ml"
  )
    | UNICODE of (
# 9 "./lib/Parser.mly"
       (int * int)
# 16 "./lib/Parser.ml"
  )
    | STRING of (
# 8 "./lib/Parser.mly"
       (string)
# 21 "./lib/Parser.ml"
  )
    | STAR
    | SLASH
    | SEMICOLON
    | S
    | RSQUARE
    | RPAREN
    | RBRACE
    | PREFIX
    | PLUS
    | PERCENTAGE of (
# 10 "./lib/Parser.mly"
       (float)
# 35 "./lib/Parser.ml"
  )
    | PAGE_SYM
    | NUMBER of (
# 8 "./lib/Parser.mly"
       (string)
# 41 "./lib/Parser.ml"
  )
    | MINUS
    | MEDIA_SYM
    | LSQUARE
    | LPAREN
    | LBRACE
    | INVALID
    | INCLUDES
    | IMPORT_SYM
    | IMPORTANT_SYM
    | IDENT of (
# 8 "./lib/Parser.mly"
       (string)
# 55 "./lib/Parser.ml"
  )
    | HASH of (
# 8 "./lib/Parser.mly"
       (string)
# 60 "./lib/Parser.ml"
  )
    | GREATER
    | FUNCTION of (
# 8 "./lib/Parser.mly"
       (string)
# 66 "./lib/Parser.ml"
  )
    | EXCLAMATION
    | ERROR of (
# 12 "./lib/Parser.mly"
       (char)
# 72 "./lib/Parser.ml"
  )
    | EQUALS
    | EOF
    | DOT
    | DIMENSION of (
# 11 "./lib/Parser.mly"
       (float * string)
# 80 "./lib/Parser.ml"
  )
    | DASHMATCH
    | CONTAINS
    | COMMA
    | COLOR
    | COLON
    | CHILD
    | CHARSET_SYM
    | CHAR of (
# 8 "./lib/Parser.mly"
       (string)
# 92 "./lib/Parser.ml"
  )
    | CDO
    | CDC
    | ATPAGE
    | ATMEDIA
    | ATKEYWORD of (
# 8 "./lib/Parser.mly"
       (string)
# 101 "./lib/Parser.ml"
  )
    | ATIMPORT
    | ATFONTFACE
    | ATCHARSET
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState52
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState45
  | MenhirState42
  | MenhirState37
  | MenhirState31
  | MenhirState30
  | MenhirState27
  | MenhirState25
  | MenhirState24
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState14
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState7
  | MenhirState6
  | MenhirState1
  | MenhirState0

# 1 "./lib/Parser.mly"
  
open Types 

# 151 "./lib/Parser.ml"

let rec _menhir_goto_opt_operatorterms : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_operatorterms -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_opt_operatorterms) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_opt_operatorterms) : 'tv_opt_operatorterms) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_expr = 
# 40 "./lib/Parser.mly"
                             ( Term _1 :: _2 )
# 169 "./lib/Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_expr) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState23 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv175 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 182 "./lib/Parser.ml"
            )) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv171 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 192 "./lib/Parser.ml"
                )) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | S ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
                    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv172)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv173 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 212 "./lib/Parser.ml"
                )) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
        | MenhirState11 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv183 * _menhir_state * 'tv_property)) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IMPORTANT_SYM ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | S ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | SEMICOLON ->
                    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv178)
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
                ((let _v : 'tv_opt_prio = 
# 37 "./lib/Parser.mly"
            ( true )
# 242 "./lib/Parser.ml"
                 in
                _menhir_goto_opt_prio _menhir_env _menhir_stack _v) : 'freshtv180)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv181 * _menhir_state * 'tv_property)) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
        | _ ->
            _menhir_fail ()) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * 'tv_operator) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_opt_operatorterms) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_operator) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_opt_operatorterms) : 'tv_opt_operatorterms) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_operator)), _, (_2 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_opt_operatorterms = 
# 62 "./lib/Parser.mly"
                                   ( Operator _1 :: Term _2 :: _3 )
# 267 "./lib/Parser.ml"
         in
        _menhir_goto_opt_operatorterms _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
    | _ ->
        _menhir_fail ()

and _menhir_goto_dimension : _menhir_env -> 'ttv_tail -> 'tv_dimension -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_opt_unary_operator) * 'tv_dimension) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv170)

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_operator = 
# 66 "./lib/Parser.mly"
     ( NoOp )
# 295 "./lib/Parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_operatorterms = 
# 61 "./lib/Parser.mly"
                    ( [] )
# 304 "./lib/Parser.ml"
     in
    _menhir_goto_opt_operatorterms _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_goto_opt_unary_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_unary_operator -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_opt_unary_operator) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DIMENSION _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "./lib/Parser.mly"
       (float * string)
# 352 "./lib/Parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
        let ((n : (
# 11 "./lib/Parser.mly"
       (float * string)
# 360 "./lib/Parser.ml"
        )) : (
# 11 "./lib/Parser.mly"
       (float * string)
# 364 "./lib/Parser.ml"
        )) = _v in
        ((let _v : 'tv_dimension = 
# 57 "./lib/Parser.mly"
                 ( n )
# 369 "./lib/Parser.ml"
         in
        _menhir_goto_dimension _menhir_env _menhir_stack _v) : 'freshtv154)) : 'freshtv156)
    | NUMBER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "./lib/Parser.mly"
       (string)
# 378 "./lib/Parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
        let ((n : (
# 8 "./lib/Parser.mly"
       (string)
# 386 "./lib/Parser.ml"
        )) : (
# 8 "./lib/Parser.mly"
       (string)
# 390 "./lib/Parser.ml"
        )) = _v in
        ((let _v : 'tv_dimension = 
# 58 "./lib/Parser.mly"
              ( (float_of_string n, "") )
# 395 "./lib/Parser.ml"
         in
        _menhir_goto_dimension _menhir_env _menhir_stack _v) : 'freshtv158)) : 'freshtv160)
    | PERCENTAGE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
        let (_v : (
# 10 "./lib/Parser.mly"
       (float)
# 404 "./lib/Parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
        let ((n : (
# 10 "./lib/Parser.mly"
       (float)
# 412 "./lib/Parser.ml"
        )) : (
# 10 "./lib/Parser.mly"
       (float)
# 416 "./lib/Parser.ml"
        )) = _v in
        ((let _v : 'tv_dimension = 
# 59 "./lib/Parser.mly"
                  ( (n, "%") )
# 421 "./lib/Parser.ml"
         in
        _menhir_goto_dimension _menhir_env _menhir_stack _v) : 'freshtv162)) : 'freshtv164)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_opt_unary_operator) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _ | SEMICOLON ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_goto_opt_prio : _menhir_env -> 'ttv_tail -> 'tv_opt_prio -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv151 * _menhir_state * 'tv_property)) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    let (_v : 'tv_opt_prio) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv149 * _menhir_state * 'tv_property)) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    let ((_5 : 'tv_opt_prio) : 'tv_opt_prio) = _v in
    ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_property)), _, (_3 : 'tv_Ss)), _, (_4 : 'tv_expr)) = _menhir_stack in
    let _2 = () in
    let _v : 'tv_declaration = 
# 33 "./lib/Parser.mly"
                                      ( fun x -> (_1, _4, _5) :: x )
# 465 "./lib/Parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv147) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_declaration) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv144)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_declaration) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv146)
    | _ ->
        _menhir_fail ()) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)

and _menhir_goto_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_operator -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_operator) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | HASH _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | MINUS ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PLUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRING _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | URI _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | DIMENSION _ | NUMBER _ | PERCENTAGE _ ->
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv142)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SLASH ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | IMPORTANT_SYM | LPAREN | SEMICOLON ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | STRING _ | URI _ ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv138)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state * 'tv_operator) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SLASH ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | IMPORTANT_SYM | LPAREN | SEMICOLON ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | STRING _ | URI _ ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv140)
    | _ ->
        _menhir_fail ()

and _menhir_reduce23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_unary_operator = 
# 52 "./lib/Parser.mly"
    ( None )
# 577 "./lib/Parser.ml"
     in
    _menhir_goto_opt_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "./lib/Parser.mly"
       (string)
# 584 "./lib/Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "./lib/Parser.mly"
       (string)
# 603 "./lib/Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_opt_unary_operator = 
# 54 "./lib/Parser.mly"
        ( Plus )
# 629 "./lib/Parser.ml"
     in
    _menhir_goto_opt_unary_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_opt_unary_operator = 
# 53 "./lib/Parser.mly"
         ( Minus )
# 643 "./lib/Parser.ml"
     in
    _menhir_goto_opt_unary_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "./lib/Parser.mly"
       (string)
# 650 "./lib/Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "./lib/Parser.mly"
       (string)
# 669 "./lib/Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "./lib/Parser.mly"
       (string)
# 688 "./lib/Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "./lib/Parser.mly"
       (string)
# 707 "./lib/Parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | COLON ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_goto_Ss : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_Ss -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_Ss = 
# 75 "./lib/Parser.mly"
        ( () )
# 737 "./lib/Parser.ml"
         in
        _menhir_goto_Ss _menhir_env _menhir_stack _menhir_s _v) : 'freshtv62)) : 'freshtv64)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv66)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 757 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 763 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "./lib/Parser.mly"
       (string)
# 768 "./lib/Parser.ml"
        ))), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _v : 'tv_property = 
# 35 "./lib/Parser.mly"
                   ( _1 )
# 773 "./lib/Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_property) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_property) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_property) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | S ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | STRING _ | URI _ ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv68)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_property) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)) : 'freshtv78)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state * 'tv_property)) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FUNCTION _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | HASH _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | MINUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | STRING _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | URI _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | DIMENSION _ | NUMBER _ | PERCENTAGE _ ->
            _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv80)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 837 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 843 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "./lib/Parser.mly"
       (string)
# 848 "./lib/Parser.ml"
        ))), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _v : 'tv_term = 
# 46 "./lib/Parser.mly"
            ( URI _1 )
# 853 "./lib/Parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 861 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 867 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "./lib/Parser.mly"
       (string)
# 872 "./lib/Parser.ml"
        ))), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _v : 'tv_term = 
# 44 "./lib/Parser.mly"
               ( String _1 )
# 877 "./lib/Parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv86)) : 'freshtv88)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 885 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 891 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "./lib/Parser.mly"
       (string)
# 896 "./lib/Parser.ml"
        ))), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _v : 'tv_term = 
# 45 "./lib/Parser.mly"
              ( Ident _1 )
# 901 "./lib/Parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)) : 'freshtv92)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 909 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 915 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "./lib/Parser.mly"
       (string)
# 920 "./lib/Parser.ml"
        ))), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _v : 'tv_hexcolor = 
# 70 "./lib/Parser.mly"
                   ( _1 )
# 925 "./lib/Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_hexcolor) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_hexcolor) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_hexcolor) : 'tv_hexcolor) = _v in
        ((let _v : 'tv_term = 
# 47 "./lib/Parser.mly"
              ( HexColor _1 )
# 942 "./lib/Parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)) : 'freshtv96)) : 'freshtv98)) : 'freshtv100)) : 'freshtv102)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 950 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FUNCTION _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | HASH _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | IDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | MINUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | STRING _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | URI _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | DIMENSION _ | NUMBER _ | PERCENTAGE _ ->
            _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv104)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_operator = 
# 67 "./lib/Parser.mly"
             ( Slash )
# 985 "./lib/Parser.ml"
         in
        _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)) : 'freshtv108)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_operator = 
# 68 "./lib/Parser.mly"
             ( Comma )
# 998 "./lib/Parser.ml"
         in
        _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv115 * _menhir_state * 'tv_opt_unary_operator) * 'tv_dimension) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state * 'tv_opt_unary_operator) * 'tv_dimension) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_opt_unary_operator)), (_2 : 'tv_dimension)), _, (_3 : 'tv_Ss)) = _menhir_stack in
        let _v : 'tv_term = 
# 43 "./lib/Parser.mly"
                                   ( Dimension (_1, _2) )
# 1010 "./lib/Parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)) : 'freshtv116)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv125 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1018 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv123 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1024 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, (_1 : (
# 8 "./lib/Parser.mly"
       (string)
# 1029 "./lib/Parser.ml"
        ))), _, (_2 : 'tv_Ss)), _, (_3 : 'tv_expr)), _, (_5 : 'tv_Ss)) = _menhir_stack in
        let _4 = () in
        let _v : 'tv_css_function = 
# 72 "./lib/Parser.mly"
                                              ( _3 )
# 1035 "./lib/Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_css_function) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_css_function) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_css_function) : 'tv_css_function) = _v in
        ((let _v : 'tv_term = 
# 48 "./lib/Parser.mly"
                  ( Func _1 )
# 1052 "./lib/Parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, (_2 : 'tv_Ss)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_opt_prio = 
# 38 "./lib/Parser.mly"
                    ( false )
# 1065 "./lib/Parser.ml"
         in
        _menhir_goto_opt_prio _menhir_env _menhir_stack _v) : 'freshtv128)) : 'freshtv130)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv132)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv13 * _menhir_state) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_declaration) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_declaration) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv22)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv23 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1117 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_opt_unary_operator) * 'tv_dimension) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * 'tv_operator) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_operator) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1156 "./lib/Parser.ml"
        )) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1165 "./lib/Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1174 "./lib/Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1183 "./lib/Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1192 "./lib/Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1201 "./lib/Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * 'tv_property)) * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_property)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 8 "./lib/Parser.mly"
       (string)
# 1220 "./lib/Parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_Ss) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv60)

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_Ss = 
# 74 "./lib/Parser.mly"
     ( () )
# 1244 "./lib/Parser.ml"
     in
    _menhir_goto_Ss _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | COLON | COMMA | DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT_SYM | LPAREN | MINUS | NUMBER _ | PERCENTAGE _ | PLUS | SEMICOLON | SLASH | STRING _ | URI _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and css : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 17 "./lib/Parser.mly"
       (Types.rules option)
# 1278 "./lib/Parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        ((let _1 = () in
        let _v : (
# 17 "./lib/Parser.mly"
       (Types.rules option)
# 1308 "./lib/Parser.ml"
        ) = 
# 21 "./lib/Parser.mly"
                     ( None )
# 1312 "./lib/Parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 17 "./lib/Parser.mly"
       (Types.rules option)
# 1320 "./lib/Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 17 "./lib/Parser.mly"
       (Types.rules option)
# 1328 "./lib/Parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 17 "./lib/Parser.mly"
       (Types.rules option)
# 1336 "./lib/Parser.ml"
        )) : (
# 17 "./lib/Parser.mly"
       (Types.rules option)
# 1340 "./lib/Parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv2)) : 'freshtv4)) : 'freshtv6)) : 'freshtv8)) : 'freshtv10)
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _ ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv12))

# 219 "/Users/jake/.opam/4.06.1/lib/menhir/standard.mly"
  


# 1356 "./lib/Parser.ml"
