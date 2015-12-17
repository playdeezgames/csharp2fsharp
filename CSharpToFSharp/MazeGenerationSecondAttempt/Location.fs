(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
module Location

type Location = {Column: int; Row: int}