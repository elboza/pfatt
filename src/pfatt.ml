open Printf

let usage_msg = "pfatt [-i intUpperBound] [-l lines] [-n itemsInRow]"

let flist = ref false
let fdebug=ref false
let fdump=ref false
let fyears=ref false
let fall=ref false
let fsearch=ref ""
let flistyear=ref ""
let ffiles = ref []
let fview=ref ""
let fdelete=ref ""
let ftype=ref ""
let finsert=ref false
let anon_fun argp =
  ffiles := argp :: !ffiles

let speclist =
  [("-w", Arg.Set_string fview, "view fatt");
  ("-i", Arg.Set finsert, "insert fatt");
  ("-l", Arg.Set flist, "list fatt in current year");
  ("-L", Arg.Set_string flistyear, "list fatt in given year");
  ("-a", Arg.Set fall, "list fatt in all years");
  ("-y", Arg.Set fyears, "list all years");
  ("-D", Arg.Set fdump, "db dump all fatt in json list");
  ("-X", Arg.Set_string fdelete, "delete an item");
  ("-d", Arg.Set fdebug, "debug");
  ("-t", Arg.Set_string ftype, "output type (json, html)");
  ("-s", Arg.Set_string fsearch, "search by date")]

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
let get_current_year=
  Unix.localtime(Unix.time())|>(fun x-> x.tm_year+1900) |> string_of_int

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !fdebug=true then
    begin
      printf "fdebug: %B\n" !fdebug;
      printf "fview: %s\n" !fview;
      printf "flist: %B\n" !flist;
      printf "finsert: %B\n" !finsert;
      printf "fall: %B\n" !fall;
      printf "fyears: %B\n" !fyears;
      printf "fsearch: %s\n" !fsearch;
      printf "ffiles: [%s]\n" (String.concat "," !ffiles);
    end;
  if !flist=true then
  begin
    let db=Db.db_open in
      printf "list...\n";
      Db.list db get_current_year;
      Db.db_close db |> ignore
  end;
  if !flistyear<>"" then
  begin
    let db=Db.db_open in
      printf "list given year...\n";
      Db.list db !flistyear;
      Db.db_close db |> ignore
  end;
  if !fyears=true then
  begin
    let db=Db.db_open in
      printf "years...\n";
      Db.years_list db;
      Db.db_close db |> ignore
   end;
  if !fall=true then
  begin
    let db=Db.db_open in
      printf "list all...\n";
      Db.list db "all";
      Db.db_close db |> ignore
  end;
  if !fsearch<>"" then
  begin
    let db=Db.db_open in
      printf "search %s...\n" !fsearch;
      Db.list db !fsearch;
      Db.db_close db |> ignore
  end;
  if !fview<>"" then
  begin
    let db=Db.db_open in
      !fview|>Db.view db !ftype;
      Db.db_close db |> ignore
  end;
  if !fdelete<>"" then
  begin
    let db=Db.db_open in
      !fdelete|>Db.delete db;
      Db.db_close db |> ignore
  end;
  if !finsert=true then
  begin
    let db=Db.db_open in
    printf "insert...\n";
      !ffiles |> List.iter (fun file->
        printf "inserting %s ...\n" file;
        read_whole_file file |> Db.insert db
      );
      Db.db_close db |> ignore
  end
