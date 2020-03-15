module Synthesis

let abelar x =

  match x>12 && x<3097 && x%12=0 with 
    | true -> true
    | false ->false

let area b h =
    match b>0.0||h>0.0 with 
     |true -> b * h * 0.5
     |false -> failwith "not a positive area"
   

let zollo x =
    match x>0 with 
    |true -> x*2
    |false -> x*(-1)
    
   
let min a b =
    match a<b with 
    |true -> a
    |false -> b

let max a b =
    match a>b with 
    |true -> a
    |false -> b

let ofTime h m s = (h*60*60) + (m*60)+ (s)
   
let toTime n =   
 let hour = n/3600
 let mins = (n%3600)/60
 let secs = n%60
 match n<0  with
   | true -> (0,0,0)
   | false-> (hour,mins,secs)   
    
    

let digits x =
    let rec count x acc = 
         match x = 0 with 
         | true -> acc
         |_ -> count(x/10) (1+acc)
    match x = 0 with        
    | true -> 1
    | _-> count x 0


let minmax (a,b,c,d) = min a b |>min c |> min d, max a b |> max c |> max d
     
let isLeap x =
 match x < 1582 with
 |true ->  failwith "not a leaper"
 |false ->
 match (x%4=0 && x%100<>0) || (x%400 = 0) with
 | true -> true  
 | false-> false

       

let month x =
   match x with 
   |1 -> ("January",31)
   |2 -> ("February",28)
   |3 -> ("March",31)
   |4 -> ("April",30)
   |5 -> ("May",31)
   |6 -> ("June",30)
   |7 -> ("July",31)
   |8 -> ("August",31)
   |9 -> ("September",30)
   |10 -> ("October",31)
   |11 -> ("November",30)
   |12 -> ("December",31)
   |_ -> failwith "Not within scope"


let toBinary x =
    match x < 0 with
    |true -> failwith "Negative number"
    |false -> let rec divide a acc = 
               match a = 0 with  
               |true -> 
                 match acc with 
                 |"" -> "0"
                 |_ -> acc
               |false -> 
                     let reverse  = a%2
                     match reverse = 0 with
                        |true -> divide(a/2) ("0"+ acc)
                        |false -> divide(a/2) ("1"+ acc)
              divide x ""

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"