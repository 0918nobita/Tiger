(*
  第1部 コンパイラ基礎編
    第1章 はじめに
      1.1 モジュールとインターフェース
      - 構成要素を再利用できるように、コンパイラを多くの部分に分割する
      - 正しい抽象に至るまでの「思考ー実装ー再設計」の繰り返しによる学習経験が重要
      1.2 ツールとソフトウェア
      - 対話的実行環境「Standard ML of New Jersey」とコンパイラ「MLton」を用いる
      - 文と式を持ち、ループや if 文を持たない直接プログラムの言語のインタプリタの挙動を ML のプログラムでエミュレートする
      - 副作用を持たないインタプリタを作成することは、表示的意味論や属性文法 ( プログラミング言語が何を行うかを記述する方法 ) についての優れた入門となる
      - 与えられた部分式中のすべての print 文の引数の最大数を返す ML の関数 maxargs : stm -> int を書く
      - この言語のプログラムを「解釈」する ML の関数 interp : stm -> unit を書く
        関数型のスタイルで書くためには、(変数, 整数) の対のリストを保持して AssignStm 毎に新しいリストを生成する
      1.3 木構造のデータ構造
    第2章 字句解析 ( ソースファイルを、個々の単語あるいはトークンへと分解する )
    第3章 構文解析 ( プログラムの句構造を解析する )
    第4章 意味動作 ( 各句に対応する抽象構文木を構築する )
    第5章 意味解析 ( 変数の使用をその定義に関連づけ、式の型を検査して各句の意味を決定した上で、各句の翻訳を要求する )
    第6章 フレーム割付け ( 変数や関数やパラメータ等を、マシン依存の形式で駆動レコードへと配置する )
    第7章 翻訳 ( 特定のプログラミング言語や目的マシンのアーキテクチャに束縛されない中間表現木を生成する )
    第8章 正準化 ( 次フェーズへの準備として、式の副作用を取り出して条件分岐を整理する )
    第9章 命令選択 ( IR木の節点を、目的マシンの命令の動作に対応するようにグループ化する )
    第10章 制御フロー解析 ( 命令の並びを解析して、実行時にプログラムが辿る可能性のある制御の流れを示す制御フローグラフを生成する )
    第11章 データフロー解析 ( プログラムの変数を通る情報の流れに関する情報を収集する。たとえば生存解析は、各プログラム変数が必要とされる値を持っている箇所を計算する )
    第12章 コード生成 ( 各マシン命令中の一時的な名前をマシンレジスタで置き換える )
*)

type id = string (* 識別子 *)

datatype binop = Plus | Minus | Times | Div (* 2項演算子 *)

datatype stm = CompoundStm of stm * stm (* セミコロンで区切って複数のステートメントを並べたステートメント *)
             | AssignStm of id * exp (* 代入式 *)
             | PrintStm of exp list (* print 式 *)
     and exp = IdExp of id (* 識別子 *)
             | NumExp of int (* 数値定数 *)
             | OpExp of exp * binop * exp (* 2項演算子を用いた演算式 *)
             | EseqExp of stm * exp (* 式の並び *)

(* a := 5 + 3; b := (print [a, a - 1], 10 * a); print [b] *)
val prog = CompoundStm (
  AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(
    AssignStm("b",
      EseqExp(PrintStm [IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
        OpExp(NumExp 10, Times, IdExp "a"))),
    PrintStm [IdExp "b"]))

(*
  演習問題 1.1
  tree2 = insert (x, tree1) であれば、tree2 が使用されてい売る間にも tree1 が利用可能であるような、
  永続する関数型の二分探索木を実装する。
  1.1a 項目が見つかったなら true を返し、見つからなければ false を返す member 関数を実装せよ。
  1.1b プログラムを拡張して、次のように項目の存否のみならずキーを束縛へ写像する関数を含むようにせよ。
    datatype 'a tree = ...
    insert: 'a tree * key * 'a -> 'a tree
    lookup: 'a tree * key -> 'a
*)

type 'a table = {key: string, value: 'a}

datatype 'a tree = LEAF | TREE of 'a tree * 'a table * 'a tree

val empty = LEAF

(* insert :: ('a table, 'a tree) -> 'a tree  *)
fun insert (elem, LEAF) = TREE (LEAF, elem, LEAF)
  | insert (elem, TREE (l, e, r)) =
      if (#key elem) < (#key e) then TREE (insert (elem, l), e, r)
      else if (#key elem) > (#key e) then TREE (l, e, insert (elem, r))
      else TREE (l, elem, r)

(* member :: string -> 'a tree -> bool *)
fun member _ LEAF = false
  | member key (TREE (l, e, r)) = ((#key e) = key) orelse (member key l) orelse (member key r)

(* lookup :: ('a tree, key) -> 'a *)
