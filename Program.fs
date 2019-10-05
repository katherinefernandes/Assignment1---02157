(*Adrienne Cohrt  - s184426
Katherine Cardoso Petulante Fernandes - s184454
Viktor Anzhelev Tsanev - s184453*)

type Lid = string
type Flight = string 
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list;;

let A = [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])];;

(*##########################################################################################################################*)

(*Function one*)
let rec findRoute lidToFind = function
       | [] -> failwith "No Route Found"
       | (lid,route)::x when lidToFind = lid -> route
       | x::xs -> findRoute lidToFind xs;;

// This test uses the first branch as the Lid is not present in the LuggageCatalogue. Returns "No Route Found"
let Route1 = findRoute "AB 123-456" A;;
// This test uses the second branch first. Returns [("DL 189", "ATL"); ("DL 124", "BRU"); ("SN 733", "CPH")].
let Route2 = findRoute "DL 016-914" A;; 
// This test uses the third branch first. Returns [("SK 208", "ATL"); ("DL 124", "BRU"); ("SK 122", "JFK")].
let Route3 = findRoute "SK 222-142" A;;

(*##########################################################################################################################*)

(*Function two*)
// The function inRoute uses an output from the function findRoute as an argument.
let rec inRoute flightToFind = function
      |[] -> false
      |(flight,airport)::xs when flightToFind = flight -> true
      |x::xs -> inRoute flightToFind xs

// This test utilizes the first branch. Returns false.
inRoute "SK 122" Route2;;
// This test first utilizes the second branch. Returns true.
inRoute "DL 189" Route2;;
// This test first utilizes the third branch. Returns true.
inRoute "SK 122" Route3;;

(*##########################################################################################################################*)

(*Function three
This function takes a flight number as an argument and returns a list of strings containing corresponding Lids*)
let rec withFlight f = function
      | [] -> []
      | (lid,route)::lc when inRoute f route = true -> lid :: withFlight f lc
      | lc::xs -> withFlight f xs;;

// This test utilizes the first branch since there is no flight with this number. Returns an empty list.
withFlight "AB 123" A;;
// This test uses both the second and third branches as the flight number appears under two Lids. Returns val it : string list = ["DL 016-914"; "SK 222-142"]
withFlight "DL 124" A;;
// This test utilizes the third branch first. Returns val it : string list = ["SK 222-142"]
withFlight "SK 208" A;;

(*##########################################################################################################################*)

(*Function four
This function set takes an arrivalCatalogue(AC) that is populated with Airports and empty lists and fills them for the 
corresponding airports from a specific route with the Lid from that route.*)

let AC = [("ATL", []); ("BRU",[]); ("JFK", []); ("CPH", [])]

// The helperFunction takes an airport code as an argument. When it finds the argument in the specified route it returns a true boolean value for that airport code.
let rec helperFunction airportToFind = function
    |[] -> false
    |(flight,airport)::xs when airportToFind = airport -> true
    |x::xs -> helperFunction airportToFind xs;;

//Converts a string into a list.
let convertLid lid = [lid]

//This helper function takes 3 arguments and creates a list with the last two arguments forming a list within a list. 
let makeAList airport lid allLids = (airport, allLids @ (convertLid lid));;

(*The extend function takes the list from makeAList when the helperFunction returns true and adds it to the arrivalCatalogue. 
If the helperFunction returns false then the corresponding airport and its empty list is removed from the arrivalCatalogue*)
let rec extend lid route = function
    |[]-> []
    |(airport, allLids)::ac when helperFunction airport route = true -> (makeAList airport lid allLids) :: extend lid route ac
    |ac::xs -> extend lid route xs;;

let ac1 = extend "DL 016-914" Route2 AC;;
let ac2 = extend "SK 222-142" Route3 AC;;

(*##########################################################################################################################*)

(*Function five*)

(*Since we coul not properly use List.fold, we did not get the expected output, but our output is a list with tuples containing the airport and the lid*)

let rec toArrivalCatalogue = function
    |[] -> []
    |(lid, route)::xs -> (extend lid route AC) @ toArrivalCatalogue xs;;

toArrivalCatalogue A;;


(*##########################################################################################################################*)

(*Function two - Question 3
The higher-order function Lists.exists was chosen over List.forall because it checks each element in the list 
individally, and it returns a boolean value based on if any of them match, as opposed to if all of them match.*)

//The helper function flightList takes a route and returns a list of flightnumbers for that route.
let rec flightList = function
    |[]->[]
    |(flight,airport)::xs -> flight :: flightList xs
//flightList Route2;; returns val it : string list = ["DL 189"; "DL 124"; "SN 733"]
flightList Route2;;
    
//The function inRoute3 uses r from the helperfunction flightList and the library function List.exists to return a boolean value corresponding to the argument given.
let rec inRoute3 flightToFind r = List.exists (fun elem -> elem = flightToFind) (flightList r)

//inRoute3 "DL 124" Route2;; returns val it : bool = true because DL 124 is present in Route2.
inRoute3 "DL 124" Route2;;

(*##########################################################################################################################*)

(*Function three - Question 3
We chose the higher-order function List.filter over List.tryFind because it will return every instance 
that matches as opposed to only some of them.*)

//The function withFlight2 takes a flight number as an argument and the luggagecatalogue and uses List.Filter identify every instance of a match with a boolean value. 
let rec withFlight2 flightToFind lc = List.filter (fun (lid, list) -> inRoute3 flightToFind list = true) lc

//The Lid function peels the Lid off of each luggageCatalogue element and puts them in a list.
let rec Lid f = function
    | [] -> []
    | (x1,x2)::xs -> f x1 @ Lid f xs;;

//The withFlight3 function takes the list of Lids from Lid f and for every true boolean value from withFlight2 it outputs the corresponding lid into a list.
let withFlight3 f lc = Lid (fun x1 -> x1::[]) (withFlight2 f lc);;

//withFlight3 "DL 124" A;; returns val it : string list = ["DL 016-914"; "SK 222-142"]
withFlight3 "SN 733" A;;
