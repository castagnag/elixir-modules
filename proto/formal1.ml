type behaviour =
  | BParam of string | BType of string * string list * Core.t
  | BOpaque of string * string list | BCallback of string * Core.t

type modul =
  | MParam of string | MParamE of string * Core.t
  | MType of string * string list * Core.t | MBehaviour of string
  | MDef of bool * string * (string * Core.t) list * Core.t * Core.e

type program =
  | M of modul list
  | B of behaviour list
