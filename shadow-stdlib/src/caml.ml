
  module Arg = Arg 

  module StdLabels = StdLabels 

  module Bool = Shadow_bool 

  module Buffer = Buffer 

  module Char = Char 

  module Float = Shadow_float 

  module Format = Format 

  module Fun = Shadow_fun 

  module MoreLabels = MoreLabels

  module Int32 = Int32 

  module Int = Shadow_int 

  module Int64 = Int64 

  module Lazy = Lazy 

  module Lexing = Lexing 

  module Nativeint = Int32

  module Obj = Obj 

  module Option = Shadow_option 

  module Parsing = Parsing 

  module Printexc = Printexc 

  module Printf = Printf 

  module Queue = Queue 

  module Random = Random 

  module Result = Shadow_result 

  module Scanf = Scanf 

  module Seq = Shadow_seq 

  module Set = MoreLabels.Set 

  module Stack = Stack 

  module Stream = Stream 

  module String = StdLabels.String 

  module Sys = Shadow_sys 

  module Uchar = Uchar 

  module Unit = Shadow_unit 

  include Pervasives [@owarning "-3"]

  exception Not_found = Not_found