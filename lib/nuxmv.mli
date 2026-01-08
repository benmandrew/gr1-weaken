type check_result =
  | Valid
  | Invalid of string (* XML counterexample *)
  | Error of string (* stdout/stderr from NuSMV *)

val check : string -> Gr1.t -> check_result
