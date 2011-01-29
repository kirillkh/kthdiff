(*let a = [| 1; 2; 3; 4; 6; 8 |]
let b = [| 1; 2; 3;   5     |]*)
let ax = [|         -1; 0; 1; 2; 3; 4; 6; 7; 8; 9; 10 |]
let bx = [| -3; -2;        1; 2; 3;   5;     8;       11; 12 |]
let blank = -1

let y' = 0
let y = ref y'

type 'a sform_matrix = Mx of int * (int->(int*int)) * (int->int->'a)

module type Num = sig
  type t
  val blank : t
  val cmpz : t -> int
  val diff : t -> t -> t
  val neg : t -> t
end

module Make : functor(Elt : Num) ->
sig
  val make : Elt.t array -> Elt.t array -> Elt.t sform_matrix
end = functor(Elt : Num) ->
struct
  type t = Elt.t
  let blank, cmpz, diff, neg = Elt.blank, Elt.cmpz, Elt.diff, Elt.neg
  type bld = { f:int->int->t; n:int; m:int }
  
  (* Returns the negated upper part of (B-A) matrix (the part above its dividing ladder). *)
  let upper ({f=f} as mx) = { mx with f = fun i j ->
                              let v = f i j in if cmpz v<0 then neg v else blank }
  (* Same as above, for the lower part. *)
  let lower ({f=f} as mx) = { mx with f = fun i j ->
                              let v = f i j in if cmpz v>=0 then v else blank }

  (* Reverses the rows order in the given matrix. *)
  let rev_rows ({f=f; m=m} as mx) = { mx with f = (fun i j -> f (m-i-1) j) }
  let rev_cols ({f=f; n=n} as mx) = { mx with f = (fun i j -> f i (n-j-1)) }

  (* Sticks two |B|x|A| matrices together into a 2|B|x2|A| matrix. *)
  let stick {f=f1; m=m1; n=n1} {f=f2; m=m2; n=n2} =
    {
      m = m1+m2;
      n = n1+n2;
      f = fun i j ->
        if i<m1 && j<n1 then (f1 i j)
        else if n1<=j && m1<=i then (f2 (i-m1) (j-n1))
        else blank
    }                       
      

  (* Rearranges the given (B-A) matrix into a standard form 2|B|x2|A| matrix of pairwise distances *)
  let convert mx = stick (rev_cols (lower mx)) (rev_rows (upper mx))


  (* Explicitly written convert. *)
  let convert2 f m n = {
    m = 2*m;
    n = 2*n;
    f = fun i j ->
      if i<m && j<n then 
        let d = f i (n-j-1) in
        if cmpz d>=0 then d else blank
      else if m<=i && n<=j then
        let d = neg (f (2*m-i-1) (j-n)) in
        if cmpz d>0 then d else blank
      else blank
  }


  let mk_bounds {m=m; n=n; f=f} =
    let bounds = Array.make (2*m) (-1,-1) in
    
    let rec scan i j =
      if i=m then bounds
      else if j>=n || cmpz (f i j)<0 then (
        let blstart, blend =
          if j=0 then 0,0 else n-j, n
        in let bustart, buend =
            if j=n then 0,0 else n+j, 2*n
        in
          bounds.(i) <- (blstart,blend);
          bounds.(2*m-i-1) <- (bustart,buend);
          scan (i+1) j
      ) else
        scan i (j+1)
    in scan 0 0


  let make a b =
    let startmx = { m=Array.length b; n=Array.length a;
                       f=fun i j -> diff b.(i) a.(j) } in
    (* let mx = convert startmx in *)
    let mx = convert2 startmx.f startmx.m startmx.n in
    let bounds = mk_bounds startmx in
    Mx (mx.m, (fun i -> bounds.(i)), mx.f)
end



module Int = struct
  type t = int
  let blank = -1
  let cmpz x = x
  let diff x y = x - y
  let neg x = -x
end
  
module IntMake = Make(Int)





(*
let trace z mx =
  let rec tr z i t = if i=2*m then () else (
    let (l,r) = bounds.(i) in
    if z < mx i l then () else ()
  ) in tr 0 m
*)





(* Optimized c2. *)
let sgn x = 1 - (x lsr 62)
let nsgn x = x lsr 62

(*
let c3 i j =
  let modi, modj = i mod m, j mod n in
  let d1 = b.(modi) - a.(n-modj-1) in
  let d2 = b.(m-modi-1) - a.(modj) in
  let im, jn = i-m, j-n in
  let f1 = (nsgn im) * (nsgn jn) * sgn(d1) in
  let f2 = (sgn im) * (sgn jn) * nsgn(d2) in
  let f3 = (sgn (-f1-f2)) in
  f1*d1 - f2*d2 + f3*blank

let c4 i j =
  let im, jn = i-m, j-n in
  let nsim, nsjn = im lsr 62, jn lsr 62 in
  let sim, sjn = 1-nsim, 1-nsjn in
  let modi, modj = im*sim + i*nsim, jn*sjn + j*nsjn in
  let d1 = b.(modi) - a.(n-modj-1) in
  let d2 = b.(m-modi-1) - a.(modj) in
  let f1 = nsim * nsjn * sgn(d1) in
  let f2 = sim * sjn * nsgn(d2) in
  let f3 = 1 - (f1 lor f2) in
  f1*d1 - f2*d2 + f3*blank

let mx i j =
  b.(i) - a.(j)
*)
  
  
let test ax bx =
  let n = Array.length ax in
  let Mx (m, _, f) = IntMake.make ax bx in
  Printf.printf "test(): n=%d, m=%d\n" n m;
  
  let rec cols i j = if j=n*2 then () else (
(*    Printf.printf "i=%d, j=%d\n" i j; *)
    let v = f i j in
    if v <> Int.blank then print_int v else print_string " ";
    print_string " ";
    y := !y + v;
    cols i (j+1);
  ) in
  
  let rec rows i = if i=m then () else (
    cols i 0;
    print_endline "";
    rows (i+1)
  ) in
    rows 0
;;


let test_bounds ax bx =
  let n = Array.length ax in
  let Mx (m, bounds, f) = IntMake.make ax bx in
  let rec cols i j bstart bend=
    let symb =
      if (j<bstart or j>=bend) && f i j=blank then " "
      else if j>=bstart && j<bend && f i j<>blank then string_of_int (f i j)
      else "x"
    in print_string symb;
    print_string " ";

    if j+1=2*n then ()
    else cols i (j+1) bstart bend;
  in
  
  let rec rows i = if i=m then () else (
    let bstart,bend = bounds i in
(*    Printf.printf "test_bounds: bstart=%d, bend=%d\n" bstart bend; *)
    cols i 0 bstart bend;
    print_endline "";
    rows (i+1)
  ) in
    rows 0
;;  



let rec testp c i = match i with
| 0 -> ()
| _ -> test c;
       testp c (i-1)
;;


(* Printf.printf "n=%d, m=%d\n" n m;; *)
test ax bx;;
(*test c2;;*)

test_bounds ax bx;;
  
(* testp c2 100000;; *)
print_endline (string_of_int  !y);
