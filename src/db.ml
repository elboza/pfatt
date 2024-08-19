exception E of string

let db="./db/fatt.db"
let yl=ref []

let output_fatt ftype s=
	match ftype with
	|"html" -> Html.output s
	|_ -> Printf.printf "%s" s
let db_open=
	Sqlite3.db_open db
let db_close db=
	Sqlite3.db_close db
let conv_to_db_date date=
	String.split_on_char '/' date |> List.map (fun x -> Printf.sprintf "%02d" (int_of_string x)) |> List.rev |> String.concat "-";;
let get_fatt_date fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "invoice" |> member "date" |> to_string |> conv_to_db_date
let get_fatt_num fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "invoice" |> member "inv_num" |> to_string
let get_pln_tot fatt=
		let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "total" |> member "total" |> to_string
let get_curr fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "total" |> member "currency" |> to_string
let get_eur_tot fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "total" |> member "subtotal_2currency" |> to_string
let get_curr2 fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "total" |> member "currency2" |> to_string
let get_bill_to fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "bill_to" |> member "name" |> to_string
let get_description fatt=
	let open Yojson.Basic.Util in
	fatt |> Yojson.Basic.from_string |> member "desc" |> to_list |> filter_member "description" |> filter_string |> List.hd
let insert db fatt=
	let date=get_fatt_date fatt in
	let fatt_num=get_fatt_num fatt in
		let sql=[%string {sql|insert into fatt(date,id,fatt) values("$(date)","$(fatt_num)",json('$(fatt |> Yojson.Safe.from_string |> Yojson.Safe.to_string)'));|sql}] in
			match Sqlite3.exec db sql with
			|Sqlite3.Rc.OK -> Printf.printf("OK")
			|a -> raise (E (Sqlite3.Rc.to_string a))
let print_list row h=
	(*Array.iter (fun s -> Printf.printf "  %-12s" s) h;*)
	let date = match row.(0) with | Some s -> s | None -> "Null" in
	let id = match row.(1) with | Some s -> s | None -> "Null" in
	let bill_to = match row.(2) with | Some s -> get_bill_to s | None -> "Null" in
	let desc = match row.(2) with | Some s -> get_description s | None -> "Null" in
	let tot = match row.(2) with | Some s -> get_pln_tot s | None -> "Null" in
	let curr = match row.(2) with | Some s -> get_curr s | None -> "Null" in
	let tot2 = match row.(2) with | Some s -> get_eur_tot s | None -> "Null" in
	let curr2 = match row.(2) with | Some s -> get_curr2 s | None -> "Null" in
		Printf.printf "%-11s %-8s %-24s %-14s %s(%s) %s(%s)\n" date id bill_to desc tot curr tot2 curr2
let list db year=
	let ss= match year with
		|"all" -> ""
		|a -> Printf.sprintf "where date LIKE '%s%%'" a
	in
	let sql=[%string {sql|select * from fatt $(ss) ORDER BY id ASC;|sql}] in
		match Sqlite3.exec db ~cb:print_list sql with
		|Sqlite3.Rc.OK -> Printf.printf("OK")
		|a -> raise (E (Sqlite3.Rc.to_string a))
let print_fatt ftype row _ =
	match row.(0) with
		|Some s -> output_fatt ftype s
		|None -> print_endline "not found."
let view db ftype search=
	let sql=[%string {sql|select fatt from fatt where id="$(search)";|sql}] in
		match Sqlite3.exec db ~cb:(print_fatt ftype) sql with
		|Sqlite3.Rc.OK -> Printf.printf("")
		|a -> raise (E (Sqlite3.Rc.to_string a))
let years_list_cb row _ =
	match row.(0) with
	|Some s -> yl:=(s|> String.split_on_char '-' |> List.hd |> int_of_string)::!yl
	|None -> ()
let years_list db=
	let sql=[%string {sql|select date from fatt;|sql}] in
		yl:=[];
		match Sqlite3.exec db ~cb:years_list_cb sql with
		|Sqlite3.Rc.OK -> !yl |> List.sort_uniq (fun a b -> a-b) |> List.iter (fun x -> Printf.printf "%d\n" x)
		|a -> raise (E (Sqlite3.Rc.to_string a))
let delete db item=
	let sql=[%string {sql|delete from fatt where id="$(item)";|sql}] in
		match Sqlite3.exec db sql with
		|Sqlite3.Rc.OK -> Printf.printf("OK")
		|a -> raise (E (Sqlite3.Rc.to_string a))