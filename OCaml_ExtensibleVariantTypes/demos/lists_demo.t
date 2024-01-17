  $ dune exec lists_demo
  val fold_left : ('e -> 'f -> 'e) -> 'e -> 'f list -> 'e = <fun>
  val init : int -> int list -> int list = <fun>
  val map : ('\141 -> '\142) -> '\141 list -> '\142 list = <fun>
  val map_square : int list -> int list -> int list = <fun>
  val next_number : int -> (int * int) list = <fun>
  val numbers : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]
  val numbers_anamorph : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1]
  val res : int * int * int = (385, 385, 285)
  val square : int -> int = <fun>
  val squares : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
  val sum_of_squares_anamorph : int = 285
  val sum_of_squares_fold : int = 385
  val sum_of_squares_map : int = 385
  val unfoldr_next_number : int list -> int -> int list = <fun>


