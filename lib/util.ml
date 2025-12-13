let tap (f : 'a -> unit) (x : 'a) : 'a =
    f x;
    x
;;

module Infix = struct
  let ( >> ) = CCFun.compose
end
