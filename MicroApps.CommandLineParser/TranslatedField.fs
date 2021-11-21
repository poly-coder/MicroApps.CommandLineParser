namespace MicroApps.CommandLineParser

//open System.Globalization

//type TranslatedField<'t> = TranslatedField of 't * Map<string, 't>

//module TranslatedField =
//    let create value = TranslatedField(value, Map.empty)
    
//    let getNeutral (TranslatedField(value, _)) = value
//    let setNeutral value (TranslatedField(_, map)) = TranslatedField(value, map)
    
//    let getTranslations (TranslatedField(_, map)) = map
//    let setTranslations map (TranslatedField(value, _)) = TranslatedField(value, map)        
    
//    let getExactTranslation cultureKey map =
//        map |> Map.tryFind cultureKey
    
//    let getAnyTranslation cultureKeys map =
//        let rec loop keys =
//            match keys with
//            | [] -> None
//            | head :: tail ->
//                match getExactTranslation head map with
//                | Some value -> Some value
//                | None -> loop tail
//        loop cultureKeys
    
//    let getExactTranslationOrNeutral cultureKey (TranslatedField(neutral, map)) =
//        map |> Map.tryFind cultureKey |> Option.defaultValue neutral

//    let getTranslation (culture: CultureInfo) field =
//        match Map.tryFind culture.Name map with
//        | Some value -> value
//        | None ->
//            match Map.tryFind culture.ThreeLetterISOLanguageName map with
//            | Some value -> value
//            | None ->
//                match Map.tryFind culture.TwoLetterISOLanguageName map with
//                | Some value -> value
//                | None -> neutral
        
//    let setTranslation map (TranslatedField(value, _)) = TranslatedField(value, map)        

//    let translateFrom (culture: CultureInfo) (field: TranslatedField<'t>) =
//        let neutral, map = field
//        match Map.tryFind culture.Name map with
//        | Some value -> value
//        | None ->
//            match Map.tryFind culture.ThreeLetterISOLanguageName map with
//            | Some value -> value
//            | None ->
//                match Map.tryFind culture.TwoLetterISOLanguageName map with
//                | Some value -> value
//                | None -> neutral

//    let currentCulture() = CultureInfo.CurrentCulture
//    let currentUICulture() = CultureInfo.CurrentUICulture
    
//    let translateFromCurrentCulture field = translateFrom (currentCulture()) field
//    let translateFromCurrentIUCulture field = translateFrom (currentUICulture()) field
