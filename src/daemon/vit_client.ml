open Lwt
open Cohttp
open Cohttp_lwt_unix

let body url = 
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code %d\n" code;
  Printf.printf "Headers %s" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
    Printf.printf "%d\n" (String.length body);
    body

let () =
  let url = Sys.argv.(1) in
  let body = Lwt_main.run (body url) in
  print_endline ("Received body\n" ^ body)
