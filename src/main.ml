let rec diff l1 l2 = match l1, l2 with
  | (k1, v1) :: xs, (k2, v2) :: ys ->
      if String.equal k1 k2 then begin
        let v1 = OpamPrinter.FullPos.Normalise.value v1 in
        let v2 = OpamPrinter.FullPos.Normalise.value v2 in
        if not (String.equal v1 v2) then begin
          Printf.printf "- %s: %s\n+ %s: %s\n\n" k1 v1 k2 v2;
        end;
        diff xs ys
      end else if List.exists (fun (k, _) -> String.equal k2 k) xs then begin
        let v1 = OpamPrinter.FullPos.Normalise.value v1 in
        Printf.printf "- %s: %s\n\n" k1 v1;
        diff xs l2
      end else if List.exists (fun (k, _) -> String.equal k1 k) ys then begin
        let v2 = OpamPrinter.FullPos.Normalise.value v2 in
        Printf.printf "+ %s: %s\n\n" k2 v2;
        diff l1 ys
      end else begin
        let v1 = OpamPrinter.FullPos.Normalise.value v1 in
        let v2 = OpamPrinter.FullPos.Normalise.value v2 in
        Printf.printf "- %s: %s\n\n+ %s: %s\n\n" k1 v1 k2 v2;
        diff xs ys
      end
  | (k, v) :: xs, [] ->
      let v = OpamPrinter.FullPos.Normalise.value v in
      Printf.printf "- %s: %s\n\n" k v;
      diff xs []
  | [], (k, v) :: ys ->
      let v = OpamPrinter.FullPos.Normalise.value v in
      Printf.printf "+ %s: %s\n\n" k v;
      diff [] ys
  | [], [] -> ()

let opam_diff x y =
  let x = OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string x)) in
  let y = OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string y)) in
  diff (OpamFile.OPAM.to_list x) (OpamFile.OPAM.to_list y)

let () =
  match Sys.argv with
  | [|_; x; y|] -> opam_diff x y
  | _ -> failwith "read the code source"
