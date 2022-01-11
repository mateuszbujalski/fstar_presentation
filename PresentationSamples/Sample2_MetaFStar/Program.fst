module Program

module P = FStar.Printf

open Json
open Serializer

let process_sample_record_1_from_json (j : json{JObject? j}) : Tot (result (x:int{x >= 5})) = 
    let r_deserialized : result sample_record_1 = deserialize j in
    match r_deserialized with
    | Valid rv -> Valid rv.l
    | Error msg -> Error msg

let process_sample_record_2_from_json (j : json{JObject? j}) : Tot (result (x:string{String.length x <= 10})) = 
    let r_deserialized : result sample_record_2 = deserialize j in
    match r_deserialized with
    | Valid rv -> Valid rv._y
    | Error msg -> Error msg

let test_sample_record_1 () =
    FStar.IO.print_string "Write json(sample_record_1):\n";
    let json_str = FStar.IO.input_line () in
    let json = Json.Parser.parse json_str in

    if JObject? json
        then begin
            let l_r = process_sample_record_1_from_json json in
            match l_r with
            | Valid l -> FStar.IO.print_string (P.sprintf "Value of l field is: %d\n" l)
            | Error msg -> FStar.IO.print_string (P.sprintf "Value of l field is not available due to: %s\n" msg)
            end
        else FStar.IO.print_string "Provided json does not represent an object\n"

let test_sample_record_2 () = 
    FStar.IO.print_string "Write json (sample_record_2):\n";
    let json_str2 = FStar.IO.input_line () in
    let json2 = Json.Parser.parse json_str2 in
    if JObject? json2
        then begin
            let y_r = process_sample_record_2_from_json json2 in
            match y_r with
            | Valid y -> FStar.IO.print_string (P.sprintf "Value of y field is: %s\n" y)
            | Error msg -> FStar.IO.print_string (P.sprintf "Value of y field is not available due to: %s\n" msg)
            end
        else FStar.IO.print_string "Provided json does not represent an object\n"

let main = 
    test_sample_record_1 ();
    test_sample_record_2 ()