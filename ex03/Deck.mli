module rec Deck : sig
  (* Card モジュールを内包する *)
  module Card : sig
    module Color : sig
      type t = Spade | Heart | Diamond | Club

      val all : t list
      val toString : t -> string
      val toStringVerbose : t -> string
    end

    module Value : sig
      type t =
        | T2
        | T3
        | T4
        | T5
        | T6
        | T7
        | T8
        | T9
        | T10
        | Jack
        | Queen
        | King
        | As

      val all : t list
      val toInt : t -> int
      val toString : t -> string
      val toStringVerbose : t -> string
      val next : t -> t
      val previous : t -> t
    end

    type t

    val newCard : Value.t -> Color.t -> t
    val allSpades : t list
    val allHearts : t list
    val allDiamonds : t list
    val allClubs : t list
    val all : t list
    val getValue : t -> Value.t
    val getColor : t -> Color.t
    val toString : t -> string
    val toStringVerbose : t -> string
    val compare : t -> t -> int
    val max : t -> t -> t
    val min : t -> t -> t
    val best : t list -> t
    val isOf : t -> Color.t -> bool
    val isSpade : t -> bool
    val isHeart : t -> bool
    val isDiamond : t -> bool
    val isClub : t -> bool
  end

  (* デッキを表す抽象型 *)
  type t

  (* ランダムな順序の52枚のデッキを生成 *)
  val newDeck : unit -> t

  (* デッキのカードを文字列のリストとして取得 *)
  val toStringList : t -> string list

  (* デッキのカードを詳細な文字列のリストとして取得 *)
  val toStringListVerbose : t -> string list

  (* カードを1枚引く。デッキが空の場合は Failure 例外を投げる *)
  val drawCard : t -> Card.t * t
end
