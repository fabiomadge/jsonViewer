{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.F

  type id = int
    deriving (Json)
  type bdaddr = string
    deriving (Json)
  type comment = string
    deriving (Json)
  type version = string
    deriving (Json)
  type device = bdaddr * comment * version
    deriving (Json)
  type record = id * device
    deriving (Json)
  type db = record list
    deriving (Json)
  type message = Delete of id | Create of device
    deriving (Json)

  let printDevice (bdaddr, comment, version) = String.concat "; " [bdaddr; comment; version]
}}

let bus = Eliom_bus.create Json.t<db>


module JsonViewer_app =
  Eliom_registration.App (
    struct
      let application_name = "jsonViewer"
    end)


let rec getNextRand xs : id list = match xs with
  | [] -> Random.int 1073741823 :: []
  | (x::xs) -> let randomNumber = Random.int 1073741823 in
    if List.exists (fun x -> x == randomNumber) xs then randomNumber :: xs
    else getNextRand xs

let rec addDevices devs prevs : db = match devs with
  | [] -> []
  | (h::t) -> let newNum = List.hd (getNextRand prevs)in
    (newNum, h) :: (addDevices t (newNum::prevs))

let initatateDevList ns : db = addDevices ns []

let toDeviceList json : device list =
  let open Yojson.Basic.Util in
  let ex args key = args |> member key |> to_string in
  List.map (fun (s,args) -> (s, ex args "comment", ex args "prerelease")) (to_assoc json)


let readDevices name : device list =
  let open Yojson.Safe in
  toDeviceList (to_basic (from_file name))

let devices =
  Eliom_reference.eref
    ~scope:Eliom_common.global_scope
    (initatateDevList (readDevices "devices.js"))


let writeDevices name =
  let open Yojson.Safe in
  let open Printf in
  let jsField name value = (name, `String value) in
  let recordToJson (addr, comment, version) =
    (addr, `Assoc [jsField "comment" comment; jsField "prerelease" version]) in
  lwt devs = Eliom_reference.get devices in
  let jsonString = pretty_to_string (`Assoc (List.map recordToJson (List.map snd devs))) in
  let oc = open_out name in
  fprintf oc "%s\n" jsonString;
  close_out oc;
  Lwt.return ()


let main_service =
  Eliom_service.App.service 
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()


{server{
  let addDev ((bd,comment,version) as dev) =
    lwt devs = Eliom_reference.get devices in
    let newDB = (List.append devs (addDevices [dev] (List.map fst devs))) in
    Eliom_lib.debug "Device added";
    Eliom_reference.set devices newDB;
    writeDevices "devices.js";
    Eliom_bus.write bus newDB


  let deleteDev id =
    lwt devs = Eliom_reference.get devices in
    let newDB = (List.filter (fun (oid, _) -> not (id == oid)) devs) in
    Eliom_lib.debug "Device removed";
    Eliom_reference.set devices newDB;
    writeDevices "devices.js";
    Eliom_bus.write bus newDB

  let messageHandler m = match m with
    | Delete record -> deleteDev record
    | Create id -> addDev id

  let rpc_message = server_function Json.t<message> messageHandler

  let log str = Lwt_io.write_line Lwt_io.stdout str

  let rpc_log = server_function Json.t<string> log
}}

{client{
  let document = Dom_html.window##document

  let button name callback =
    let res = document##createDocumentFragment() in
    let input = Dom_html.createInput ~_type:(Js.string "submit") document in
    input##value <- Js.string name;
    input##onclick <- Dom_html.handler callback;
    Dom.appendChild res input;
    res

  let text_box reference =
    let res = document##createDocumentFragment() in
    let input = Dom_html.createInput ~_type:(Js.string "text") document in
    input##value <- Js.string (!reference);
    input##onchange <- Dom_html.handler
      (fun _ ->
        begin try
          reference := (Js.to_string (input##value))
        with Invalid_argument _ ->
          ()
        end;
       input##value <- Js.string (!reference);
       Js._false);
    Dom.appendChild res input;
    res

  let ul_skeleton =
    let res = document##createDocumentFragment() in
    let ul = Dom_html.createUl document in
    ul##id <- Js.string ("list");
    Dom.appendChild res ul;
    res

  let message_rpc = %(server_function Json.t<message> messageHandler)

  let sendDevice addr comment version = message_rpc (Create (!addr, !comment, !version))

  let deleteDevice id = message_rpc (Delete id)

  let reload = ()

  let easyAddLi ((id, device) : record) ul = 
    let li = Dom_html.createLi document in
    let delete = Dom_html.createInput ~_type:(Js.string "submit") document in

    delete##value <- Js.string "Delete";
    delete##onclick <- Dom_html.handler (fun _ -> deleteDevice id; Js._false);

    li##innerHTML <- Js.string (printDevice device);
    li##id <- Js.string (string_of_int id);

    Dom.appendChild li delete;
    Dom.appendChild ul li



  let deleteLi li ul = Dom.removeChild ul li

  let addLi ((id, device) : record) (c : Dom.node Js.t) ul =
    let li = Dom_html.createLi document in
    li##innerHTML <- Js.string (printDevice device);
    li##id <- Js.string (string_of_int id);
    (*Probably not working*)
    Dom.insertBefore ul li (c##firstChild)

  let sameID (id, device) (li : Dom.node Js.t) =
    let el = (Js.Unsafe.coerce li)  in
    let idStr = (Js.to_string (el##getAttribute (Js.string "id"))) in
    0 == (compare (string_of_int id) idStr)

  let rec idInRestNodes record  lis = match lis with
    | [] -> false
    | (x::xs) -> sameID record x || idInRestNodes record xs


  let rec diffHelper db (children : Dom.node Js.t list) ul = match (db, children) with
    | ([], []) -> ()
    | (r::db, []) -> easyAddLi r ul;
      diffHelper db [] ul
    | ([], c::childlist) -> deleteLi c ul;
      diffHelper [] childlist ul
    | (d::db, c::childlist) -> if sameID d c
      then diffHelper db childlist ul
      else if idInRestNodes d (c::childlist)
        then
          (*Sorry, but the compliler wouldn't take it another way*)
          (*let a = deleteLi c ul in*)
          diffHelper (d::db) childlist ul
        else
          (*let a = addLi d c ul in*)
          diffHelper db (c::childlist) ul


  let diffList db = 
    let ul =
      Js.Opt.get (document##getElementById(Js.string "list"))
        (fun () -> assert false)
    in
    let childlist = Dom.list_of_nodeList (ul##childNodes) in
    diffHelper db childlist ul
    (*addLi (List.hd db) (List.hd childlist) ul*)


  let updateList (db : db) = diffList db

  let addButton li = 
    let el = (Js.Unsafe.coerce li)  in
    let idStr = (Js.to_string (el##getAttribute (Js.string "id"))) in
    let delete = Dom_html.createInput ~_type:(Js.string "submit") document in
    delete##value <- Js.string "Delete";
    delete##onclick <- Dom_html.handler (fun _ -> deleteDevice (int_of_string idStr); Js._false);
    Dom.appendChild li delete

  let initList ul =
    let childlist = Dom.list_of_nodeList (ul##childNodes) in
    List.map addButton childlist;
    ()

  let init_client () = 
    let main =
      Js.Opt.get (document##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    let ul =
      Js.Opt.get (document##getElementById(Js.string "list"))
        (fun () -> assert false)
    in
    let addr = ref "00-12-6f-10-00-00" in
    let comment = ref "Lars, Capsense 2 (rot)" in
    let version = ref "1.5-rc1" in
    (*Dom.appendChild main ul_skeleton;*)
    Dom.appendChild main (text_box addr);
    Dom.appendChild main (text_box comment);
    Dom.appendChild main (text_box version);
    Dom.appendChild main (button "Add Device" (fun _ -> sendDevice addr comment version; Js._false));
    (*Dom.appendChild main (button "Reload Server File" (fun _ -> reload; Js._false));*)
    ignore (Lwt_js.sleep 0.1 >>= fun () -> (* avoid chromium looping cursor *)
        Lwt.catch
          (fun () ->
            Lwt_stream.iter updateList (Eliom_bus.stream %bus))
          (function e (* Eliom_comet.Channel_full *) ->
            Firebug.console##log (e);
            Eliom_client.exit_to
              ~service:Eliom_service.void_coservice' () ();
            Lwt.return ()));
    initList ul
    
}}


let _ =
  JsonViewer_app.register
    ~service:main_service
    (fun () () ->
      ignore {unit{ init_client () }};
      lwt devs = Eliom_reference.get devices in
      Eliom_lib.debug "Load page";
      Lwt.return
        (Eliom_tools.F.html
           ~title:"jsonViewer"
           Html5.F.(body [
              div ~a:[a_id "main"] 
                [ul
                  ~a:[a_id "list"]
                  (List.map (fun (id, device) -> li ~a:[a_id (string_of_int id)] [pcdata (printDevice device)]) devs)
                ] 
           ])))

