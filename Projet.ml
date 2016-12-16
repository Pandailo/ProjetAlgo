 
module L = List;;
module A = Array;;
module P = Printf;;

(* ========================================================
 * Retourne a à la puissnace b
 * ========================================================*)
let rec pow a b =
	if 0=b then 1
	else
	a*pow a (b-1);;
	
(*=========================================================
 * Retourne  le degré du polynome
 * ========================================================*)
let deg_poly p= L.length p -1;;

(* ========================================================
 * Retourne vraie si le polynome a est plus 
 * grand que b, sinon retourne faux
 * ========================================================*)
let rec biggerThan_ a b=
	match a,b with
	|[],[]-> true
	|[],_->false
	|_,[]->true
	|[x],[y]->x>y
	|t1::q1,t2::q2-> if t1=t2 then biggerThan_ q1 q2 else t1>t2
let biggerThan a b=
	if (deg_poly a)>(deg_poly b)
	then true
	else if (deg_poly a)<(deg_poly b)
	then false
	else biggerThan_ (L.rev a) (L.rev b);;
	
(* ========================================================
 * Retire les zeros se trouvant a la fin du polynome
 * (donc au debut du nombre)
 * ========================================================*)
let rec normalize_ a=
	match a with
	|[]->[]
	|[x]->[x]
	|t::q-> if 0=t then normalize_ q else a;;
let normalize a=
	L.rev (normalize_ (L.rev a));;

(* ========================================================
 * Retourne le polynome d'un entier (poids faible a gauche)
 * ========================================================*)
let rec polynome_ a l base=
	if 0=a
	then l
	else polynome_ (a/base) (l@[a mod base]) base;;
let polynome a base=
	polynome_ a [] base;;
	
(* ========================================================
 * Retourne l'entier du polynome
 * ========================================================*)
let rec entier_ l base puiss=
	match l with
	|[]->0
	|[a]->a*(pow base puiss)
	|t::q->t*(pow base puiss) + entier_ q base (puiss+1);;
let entier l base=
	entier_ l base 0;;

(* ========================================================
 * Retourne deux polynomes à partir d'une découpe à la position n
 * ========================================================*)
let rec cut_ l1 l2 n=
	match n,l2 with
	|0,_ ->  l1,l2
	|_,[] ->  l1, []
	|n,t::q -> cut_ (l1@[t]) q (n-1);;	
let cut l n=
	cut_ [] l n;;
	
(* ========================================================
 * Retourne un polynôme négatif à partir de a
 * ========================================================*)
let rec poly_neg a=
	match a with
	|[]->[]
	|[x]->[-x]
	|t::q->(-t)::(poly_neg q);;


(* ========================================================
 * Additionne deux polynômes
 * ========================================================*)
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

(* =========================================================
 * Soustrait deux polynômes (soustraire = additionner par un négatif
 * =========================================================*)
let rec poly_sub_ a b base ret=
	match a,b with
	|[],[] -> []
	|[],_  -> failwith "Le deuxime nombre est plus grand que le premier"
	|[x],[] -> [x-ret]
	|t::q,[] -> if ret>t then (t+base-ret)::(poly_sub_ q [] base 1) else (t-ret)::q
        |[x],[y]-> if 0<>(x-y-ret) then [x-y-ret] else []
	|t1::q1,t2::q2->if (t2+ret)>t1 then (t1+base-(t2+ret))::(poly_sub_ q1 q2 base 1) else (t1-t2-ret)::(poly_sub_ q1 q2 base 0);;
let poly_sub a b base=
	if (biggerThan b a) then poly_neg (normalize (poly_sub_ b a base 0)) else normalize(poly_sub_ a b base 0);;
	
let rec poly_mult_zero a b=
        match b with
        |[]->a
        |[_]->a
        |_::q->0::(poly_mult_zero a q);;

(* ========================================================
 * multiplication d'un polynôme par un entier simple 
 * ========================================================*)
let rec poly_mult_ a b base ret=
	match a with
	|[]->if 0<>ret then [ret] else []
	|t::q->((t*b+ret) mod base)::(poly_mult_ q b base ((t*b+ret) / base));;
let poly_mult a b base=
	poly_mult_ a b base 0;;
	
	

(* ========================================================
 * KARATSUBA 
 * a0=p1*q1
 * a1=p2*q2
 * a2=(p1+q1)*(p2+q2)-a0-a1
 * Res=a0*base^2k+a2*base^k+a1
 * ========================================================*)
let rec karatsuba p q base=
	match p,q with
        |[],[]-> []
	|[],_ -> q
        |_,[] -> p
        |[a],[b]->if (a*b)>base then ((a*b) mod base)::[(a*b)/base] else [a*b]
        |[a],_ -> poly_mult q a base
        |_,[b] -> poly_mult p b base
        |_,_ -> let k = ((min (deg_poly p) (deg_poly q))+1)/2 in
                let p2,p1 = cut p k in
                let q2,q1 = cut q k in
                let a0 = karatsuba p1 q1 base in
                let a1 = karatsuba p2 q2 base in
                let a2 = poly_sub (poly_sub (karatsuba (poly_add p1 p2 base) (poly_add q1 q2 base) base) a0 base) a1 base in
                (poly_add (poly_add (poly_mult_zero a0 (polynome (pow base (2*k)) base)) (poly_mult_zero a2 (polynome (pow base k) base)) base) a1 base);;
                
                
(* ======================================================== 
 * Nombres rationnels 
 * ========================================================*)


(* ======================================================== 
 * Pars d'un rationnel pour constituer deux polynômes (TODO)
 * ========================================================*)
 let rec poly_rat_ a l1 l2 base = 
 	if 0=a
	then l1 l2
	else if (a>1) 
		then (poly_rat_ (a-(a/base *base)) (l1@[a mod base]) l2 base)
	else poly_rat (a*base) l1 (l2@[(a*base) mod base]) base;;

(* ========================================================
 * Divise deux polynome, retourne le quotient et le reste
 * ========================================================*)
let rec poly_div_ a b base q=
        if (biggerThan a b) 
        then poly_div_ (poly_sub a b base) b base (q+1) 
        else (polynome q base),a;;
let poly_div a b base=
        let q,r = poly_div_ a b base 0 in
        q;;
let poly_mod a b base=
        let q,r = poly_div_ a b base 0 in
        r;;


(* ========================================================
 * PGCD (EN TRAVAUX) TODO
 * ========================================================*)
 
(*let rec pgcd a b base=
 	match a with
 	|[] -> failtwith "liste vide"
 	|[0] -> b
 	|t::q -> pgcd (poly_mod b a base) a ;;
*)	 
(* ========================================================
 * Zone de test
 * ========================================================*)
#trace karatsuba;;
let base=10;;
let polyA=[1;2;3;4;5;6;7;8;9];;
let polyB=[9;8;6;5;2;4;7;6;1];; 
let plop = karatsuba polyA polyB base;;
entier polyA base;;
entier polyB base;;
entier plop base;;
987654321*167425689;;

let polyC=polynome 525 10;;
let polyD=polynome 100 10;;
let divi=poly_div polyC polyD base;;
let modu=poly_mod polyC polyD base;;
let verif=poly_add (karatsuba polyD divi base) modu base;;
let tet=poly_rat_ 12.5 [] [] 10;;

