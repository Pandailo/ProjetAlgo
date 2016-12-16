 
module L = List;;
module A = Array;;

(*Retourne a à la puissnace b*)
let rec pow a b =
	if 0=b then 1
	else
	a*pow a (b-1);;

(* Retourne le polynome d'un entier (poids faible a gauche)*)
let rec polynome_ a l base=
	if 0=a
	then l
	else polynome_ (a/base) (l@[a mod base]) base;;
let polynome a base=
	polynome_ a [] base;;
	
(*Retourne l'entier du polynome*)
let rec entier_ l base puiss=
	match l with
	|[]->0
	|[a]->a*(pow base puiss)
	|t::q->t*(pow base puiss) + entier_ q base (puiss+1);;
let entier l base=
	entier_ l base 0;;

(*Retourne  le degré du polynome*)
let deg_poly p= L.length p -1;;

(*Retourne deux polynomes à partir d'une découpe à la position n*)
let rec cut_ l1 l2 n=
	match n,l2 with
	|0,_ ->  l1,l2
	|_,[] ->  l1, []
	|n,t::q -> cut_ (l1@[t]) q (n-1);;	
let cut l n=
	cut_ [] l n;;
	
(* Retourne un polynôme négatif à partir de a *)
let rec poly_neg a=
	match a with
	|[]->[]
	|[x]->[-x]
	|t::q->(-t)::(poly_neg q);;


(* additionne deux polynômes *)
let rec poly_add_ a b base ret=
	match a,b with
	|[],[] -> [ret]
	|[],[y] -> if (y+ret)>=base then ((y+ret) mod base)::[(y+ret)/10] else [y+ret]
	|[x],[] -> if (x+ret)>=base then ((x+ret) mod base)::[(x+ret)/10] else [x+ret]
	|t::q,[] -> if(ret>0) then ((t+ret) mod base)::(poly_add_ q b base ((t+ret) / base)) else a
	|[],t::q -> if(ret>0) then ((t+ret) mod base)::(poly_add_ q a base ((t+ret) / base)) else b
        |[x],[y]->if (x+y+ret)>=base then ((x+y+ret) mod base)::[(x+y+ret)/base] else [x+y]
	|t1::q1,t2::q2->((t1+t2+ret) mod base)::(poly_add_ q1 q2 base ((t1+t2+ret) / base));;
	
let poly_add a b base=
	poly_add_ a b base 0;;

(* Soustrait deux polynômes (soustraire = additionner par un négatif *)
let poly_sub a b base=
	polynome (entier (poly_add_ a (poly_neg b) base 0) base) base;;
	
let rec poly_mult_zero a b=
        match b with
        |[]->a
        |[_]->a
        |_::q->0::(poly_mult_zero a q);;

(* multiplication d'un polynôme par un entier simple *)
let rec poly_mult_ a b base ret=
	match a with
	|[]->if 0<>ret then [ret] else []
	|t::q->((t*b+ret) mod base)::(poly_mult_ q b base ((t*b+ret) / base));;
let poly_mult a b base=
	poly_mult_ a b base 0;;
	
	

(* KARATSUBA *)
let rec karatsuba p q base=
	match p,q with
        |[],[]-> []
	|[],_ -> q
        |_,[] -> p
        |[a],[b]->[a*b]
        |[a],_ -> poly_mult q a base
        |_,[b] -> poly_mult p b base
        |_,_ -> let k = ((min (deg_poly p) (deg_poly q))+1)/2 in
                let p2,p1 = cut p k in
                let q2,q1 = cut q k in
                let a0 = karatsuba p1 q1 base in
                let a1 = karatsuba p2 q2 base in
                let a2 = poly_sub (poly_sub (karatsuba (poly_add p1 p2 base) (poly_add q1 q2 base) base) a0 base) a1 base in
                (poly_add (poly_add (poly_mult_zero a0 (polynome (pow base (2*k)) base)) (poly_mult_zero a2 (polynome (pow base k) base)) base) a1 base);;

let a=polynome 1523 10;;
let b=polynome 6521 10;;
let base=10;;
#trace karatsuba;;

let plop = karatsuba a b base;;
entier plop 10;;
