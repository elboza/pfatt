let css="<style>
body{
	font-size:18px;
	width: 700px;
	padding-top:30px;
	padding-bottom:30px;
	margin:auto;
}
.me{
	border:1px dashed black;
	display:inline-block;
}
.row{
	display: block;
	width: 100%;
	padding-top:2px;
	padding-bottom:2px;
}
.title{
	font-size:1.4em;
	text-align:left;
}
.table_title{
	text-align:left;
	background-color: #d4e3e5;
	margin:1px;
	margin:3px;
	padding-left:20px;
	padding-right:20px;
}
.table_items{
	text-align:left;
	margin:1px;
	margin:3px;
	padding-left:20px;
	padding-right:20px;
}
.table_rows{
	border-bottom: 1px dashed black;
}
.totals_key{
	text-align:right;
}
.totals_value{
	text-align:left;
	background-color: #FFEBEE;
	margin:1px;
	margin:3px;
	padding-left:20px;
	padding-right:20px;
}
.key{
	text-align:right;
	background-color: #d4e3e5;
	margin:1px;
	margin:3px;
	padding-left:20px;
	padding-right:20px;
}
.value{

}
.invoice{
	float:right;
	display:inline-block;
	border:1px dashed black;
}
.bill_to{
	margin-top:40px;
	margin-bottom:40px;
	border:1px dashed black;
}
.desc{
	margin-top:40px;
	margin-bottom:40px;
	border:1px dashed black;
}
.bank{
	border:1px dashed black;
}
</style>"

let number_to_string = Printf.sprintf "%G"

let get_month_from_inv_num inv_num= inv_num |> String.split_on_char '/' |> List.hd

let get_year_from_inv_num inv_num=inv_num |> String.split_on_char '/' |> List.tl |> List.hd

let title fatt=
	let open Yojson.Basic.Util in
	let inv_num=fatt |> member "invoice" |> member "inv_num" |> to_string in
	[%string {|<head><title>fatt_withsecure_$(inv_num |> get_year_from_inv_num)$(inv_num |> get_month_from_inv_num)</title></head>|}]
let me fatt=
	let open Yojson.Basic.Util in
	[%string {|<div class='me'>
<table>
<tr class='title'><th colspan='2'>$(fatt |> member "me" |> member "name" |> to_string)</th></tr>
<tr><td class='key'>P.IVA</td><td>$(fatt |> member "me" |> member "piva" |> to_string)</td></tr>
<tr><td class='key'>cod.fisc.</td><td>$(fatt |> member "me" |> member "codfisc" |> to_string)</td></tr>
<tr><td class='key'>email</td><td>$(fatt |> member "me" |> member "email" |> to_string)</td></tr>
<tr><td class='key'>address</td><td>$(fatt |> member "me" |> member "addr" |> to_string)<br>$(fatt |> member "me" |> member "addr2" |> to_string)</td></tr>
<tr><td class='key'>phone</td><td>$(fatt |> member "me" |> member "phone" |> to_string)</td></tr>
</table>
</div>|}]

let invoice fatt =
	let open Yojson.Basic.Util in
	[%string {|<div class='invoice'>
<table>
<tr class='title'><th colspan='2'>B2B invoice</th></tr>
<tr><td class='key'>date</td><td>$(fatt |> member "invoice" |> member "date" |> to_string)</td></tr>
<tr><td class='key'>No.#</td><td>$(fatt |> member "invoice" |> member "inv_num" |> to_string)</td></tr>
</table>
</div>|}]

let bill_to fatt =
	let open Yojson.Basic.Util in
	[%string {|<div class='bill_to'>
<table>
<tr class='title'><th colspan='2'>Bill To.</th></tr>
<tr><td class='key'>name</td><td>$(fatt |> member "bill_to" |> member "name" |> to_string)</td></tr>
<tr><td class='key'>address</td><td>$(fatt |> member "bill_to" |> member "addr" |> to_string)<br>$(fatt |> member "bill_to" |> member "addr2" |> to_string)</td></tr>
<tr><td class='key'>NIP</td><td>$(fatt |> member "bill_to" |> member "piva" |> to_string)</td></tr>
</table>
</div>|}]

let get_vat fatt=
	let open Yojson.Basic.Util in
		fatt |> member "total" |> member "vat" |> to_string |> float_of_string

let get_second_curr fatt=
	let open Yojson.Basic.Util in
		match fatt|> member "total" |> member "other_currency" |> to_bool with
		| true -> [%string {|<tr><td class='totals_key' colspan='4'></td><td class='table_items'>($(fatt|> member "total" |> member "subtotal_2currency" |> to_string) $(fatt|> member "total" |> member "currency2" |> to_string))</td></tr>|}]
		| _ -> ""

let get_subtotal fatt=
	let open Yojson.Basic.Util in
		fatt|> member "desc" |>to_list |> List.fold_left (fun y x-> (x|>member "total_no_vat" |> to_string |> float_of_string) +.y ) 0.0

let desc fatt =
	let open Yojson.Basic.Util in
	let width="100%" in
	let subtotal=get_subtotal fatt in
	let vat=get_vat fatt in
	let total=subtotal+.subtotal*.vat in
	let second_curr=get_second_curr fatt in
	let percent="%" in
	[%string {|<div class='desc'>
<table width='$(width)'>
<tr><th class='table_title'>code</th><th class='table_title'>description</th><th class='table_title'>Qt.</th><th class='table_title'>unit price</th><th class='table_title'>total</th></tr>
$(fatt |> member "desc" |>to_list |> List.map (
	fun x -> 
		Printf.sprintf "<tr class='table_rows'>
	<td class='table_items'>%s</td>
	<td class='table_items'>%s</td>
	<td class='table_items'>%s</td>
	<td class='table_items'>%s</td>
	<td class='table_items'>%s %s</td>
	</tr>" (x|> member "code" |> to_string) (x|> member "description" |> to_string) (x|> member "qt" |> to_string) (x|> member "unit_price" |> to_string) (x|> member "total_no_vat" |> to_string) (x|> member "currency" |> to_string)) 
|> String.concat " ")
<tr><td class='totals_key' colspan='4'>SUBTOTAL</td> <td class='table_items'>$(subtotal |> number_to_string) $(fatt|> member "total" |> member "currency" |> to_string)</td></tr>
<tr><td class='totals_key' colspan='4'>VAT</td><td class='table_items'>$(vat|>number_to_string)$(percent)</td></tr>
<tr><td class='totals_key' colspan='4'>TOTAL INCL. VAT</td><td class='totals_value'>$(total|> number_to_string) $(fatt|> member "total" |> member "currency" |> to_string)</td></tr>
$(second_curr)
</table>
</div>|}]

let bank fatt =
	let open Yojson.Basic.Util in
	[%string {|<div class='bank'>
<table>
<tr class='title'><th colspan='2'>Banking details</th></tr>
<tr><td class='key'>bank name</td><td>$(fatt |> member "bank_details" |> member "bank_name" |> to_string)</td></tr>
<tr><td class='key'>bank account</td><td>$(fatt |> member "bank_details" |> member "bank_account" |> to_string)</td></tr>
</table>
</div>|}]

let output fatt=
	Printf.printf "<html>%s <body> %s %s %s %s %s %s </body> </html>" (fatt |> Yojson.Basic.from_string |> title) css (fatt |> Yojson.Basic.from_string |> me) (fatt |> Yojson.Basic.from_string |> invoice) (fatt |> Yojson.Basic.from_string |> bill_to) (fatt |> Yojson.Basic.from_string |> desc) (fatt |> Yojson.Basic.from_string |> bank)
