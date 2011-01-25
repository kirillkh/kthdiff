let a = [| 1; 2; 3; 4; 6; 8 |]
let b = [| 1; 2; 3;   5     |]
let blank = -1
let sA = Array.length a
let sB = Array.length b

let y' = 0
let y = ref y'

(* Returns the negated upper part of (B-A) matrix (the part above its dividing ladder). *)
let u m i j =
  if (m i j) <= 0 then -(m i j) else blank
(* Same as above, for the lower part. *)
let l m i j =
  if (m i j) > 0 then (m i j) else blank
                  

(* Reverses the rows order in the given matrix. *)
let rr m i j = m (sB-i-1) j
let rc m i j = m i (sA-j-1)

(* Sticks two |B|x|A| matrices together into a 2|B|x2|A| matrix. *)
let s m1 m2 i j =
  if i<sB && j<sA then (m1 i j)
  else if sA<=j && sB<=i then (m2 (i-sB) (j-sA))
  else blank
  

(* Rearranges the given (B-A) matrix into a standard form 2|B|x2|A| matrix of pairwise distances *)
let c m i j =
  s (rc (l m)) (rr (u m)) i j


(* Explicitly written c. *)
let c2 i j =
  if i<sB && j<sA then 
    let d = b.(i) - a.(sA-j-1) in
    if d > 0 then d else blank
  else if sB<=i &&  sA<=j then
    let d = a.(j-sA) - b.(2*sB-i-1) in
    if d >= 0 then d else blank
  else blank



(* Optimized c2. *)
let sgn x = 1 - (x lsr 62)
let nsgn x = x lsr 62
   
let c3 i j =
  let modi, modj = i mod sB, j mod sA in
  let d1 = b.(modi) - a.(sA-modj-1) in
  let d2 = b.(sB-modi-1) - a.(modj) in
  let isB, jsA = i-sB, j-sA in
  let f1 = (nsgn isB) * (nsgn jsA) * sgn(d1) in
  let f2 = (sgn isB) * (sgn jsA) * nsgn(d2) in
  let f3 = (sgn (-f1-f2)) in
  f1*d1 - f2*d2 + f3*blank

let c4 i j =
  let isB, jsA = i-sB, j-sA in
  let nsisB, nsjsA = isB lsr 62, jsA lsr 62 in
  let sisB, sjsA = 1-nsisB, 1-nsjsA in
  let modi, modj = isB*sisB + i*nsisB, jsA*sjsA + j*nsjsA in
  let d1 = b.(modi) - a.(sA-modj-1) in
  let d2 = b.(sB-modi-1) - a.(modj) in
  let f1 = nsisB * nsjsA * sgn(d1) in
  let f2 = sisB * sjsA * nsgn(d2) in
  let f3 = 1 - (f1 lor f2) in
  f1*d1 - f2*d2 + f3*blank


let m i j =
  b.(i) - a.(j)
  
  
let test c =
  let d = c in
  
  let rec cols i j = if j=sA*2 then () else (
    let v = d i j in
(*    if v <> blank then print_int v else print_string " ";
    print_string " "; *)
    y := !y + v;
    cols i (j+1);
  ) in
  
  let rec rows i = if i=sB*2 then () else (
    cols i 0;
(*     print_endline ""; *)
    rows (i+1)
  ) in
    rows 0
;;


let rec testp c i = match i with
| 0 -> ()
| _ -> test c;
       testp c (i-1)
;;


(* Printf.printf "sA=%d, sB=%d\n" sA sB;; *)
(* test c3;; *)

testp c2 100000;;
print_endline (string_of_int  !y);
