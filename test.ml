module Big_nat = struct
end;;
(*Selon la base du système*)
let base =
  match Sys.word_size with
  | 32 -> 10000 
  | 64 -> 1000000000 
  | _  -> 10;;
(* grand entier naturel -> tableau d'int*)
type big_nat = int array;;
type t = big_nat;;

let zero_big = ([|0|]: big_nat)
and unit_big = ([|1|]: big_nat)
;;

let big_of_int n =
  assert (0 <= n && n < base * base);
  if n < base then ([|n|]: big_nat)
  else ([|n / base;n mod base|]: big_nat);;

base;;
big_of_int 150000000123;;
(*- : big_nat = [|150; 123|] si dépasse la base
*)
let print_big (a: big_nat) =
  let print i n =
    if i=0 then print_int n
    else if base=10 then print_int n
    else if base=10000 then Printf.printf "%04d" n
    else Printf.printf "%09d" n
  in Array.iteri print a;;
base;;
(*- : int = 10000*)
print_big [|150;123|];;
(*1500123- : unit = ()*)


let add_big (a: big_nat) (b: big_nat) =
  assert (Array.length a >= Array.length b);
  let result = ref a
  and carry  = ref 0
  and i = ref (Array.length a - 1)
  and j = ref (Array.length b - 1)
  in begin
    while !j >= 0 do
      let d = a.(!i) + b.(!j) + !carry
      in if d < base then begin
        carry := 0; a.(!i) <- d 
      end else begin
        carry := 1; a.(!i) <- d - base
      end;
      decr i; decr j; 
    done;
    while !carry > 0 do
      if !i >= 0 then begin
        let d = a.(!i) + !carry
        in if d < base then begin
          carry := 0; a.(!i) <- d 
        end else begin
          a.(!i) <- d - base
        end;
        decr i;
      end else begin
        result := Array.make (Array.length a + 1) 0;
        Array.blit a 0 !result 1 (Array.length a);
        !result.(0) <- 1; carry := 0;
      end;
    done;
    !result
  end;;

(*Sum_big fait add big dans le bon sens *)
let sum_big (a: big_nat) (b: big_nat) =
  if Array.length a >= Array.length b then
    add_big (Array.copy a) b
  else 
    add_big (Array.copy b) a
  ;;
  
let a=big_of_int 1500000230;;
sum_big(a,a);;

	
