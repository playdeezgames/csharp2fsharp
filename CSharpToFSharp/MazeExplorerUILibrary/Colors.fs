module Colors

open System.Drawing
open System

type PaletteColor = Onyx | Garnet | Gold | Emerald | Aquamarine | Sapphire | Tanzanite | Silver | Tin | Carnelian | Copper |  Jade | Turquoise | LapisLazuli | Amethyst | DarkestTone | DarkerTone | DarkTone | MediumDarkTone | MediumTone | MediumLightTone | LightTone | LighterTone | LightestTone | Transparent

let GetDrawingColor = function
    | Onyx            -> Color.FromArgb(255,  32,  32,  32)
    | Garnet          -> Color.FromArgb(255, 128,  32,  32)
    | Gold            -> Color.FromArgb(255, 128, 128,  32)
    | Emerald         -> Color.FromArgb(255,  32, 128,  32)
    | Aquamarine      -> Color.FromArgb(255,  32, 128, 128)
    | Sapphire        -> Color.FromArgb(255,  32,  32, 128)
    | Tanzanite       -> Color.FromArgb(255, 128,  32, 128)
    | Silver          -> Color.FromArgb(255, 128, 128, 128)
    | Tin             -> Color.FromArgb(255,  80,  80,  80)
    | Carnelian       -> Color.FromArgb(255,  80,  32,  32)
    | Copper          -> Color.FromArgb(255,  80,  80,  32)
    | Jade            -> Color.FromArgb(255,  32,  80,  32)
    | Turquoise       -> Color.FromArgb(255,  32,  80,  80)
    | LapisLazuli     -> Color.FromArgb(255,  32,  32,  80)
    | Amethyst        -> Color.FromArgb(255,  80,  32,  80)
    | DarkestTone     -> Color.FromArgb(255,  64,  48,  48)
    | DarkerTone      -> Color.FromArgb(255,  80,  56,  56)
    | DarkTone        -> Color.FromArgb(255,  96,  64,  64)
    | MediumDarkTone  -> Color.FromArgb(255, 112,  72,  72)
    | MediumTone      -> Color.FromArgb(255, 128,  80,  80)
    | MediumLightTone -> Color.FromArgb(255, 160,  96,  96)
    | LightTone       -> Color.FromArgb(255, 192, 112, 112)
    | LighterTone     -> Color.FromArgb(255, 224, 128, 128)
    | LightestTone    -> Color.FromArgb(255, 255, 144, 144)
    | Transparent     -> Color.FromArgb(  0,   0,   0,   0)

let SkinTones = [DarkestTone; DarkerTone; DarkTone; MediumDarkTone; MediumTone; MediumLightTone; LightTone; LighterTone; LightestTone]

let RandomSkinTone() =
    let random = new System.Random()
    SkinTones 
    |> List.sortBy(fun elem -> random.Next()) 
    |> List.head