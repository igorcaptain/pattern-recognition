open Image
open Classifier

(*
let clasify clasifyFunction filePathes refAvg =
    let result =
        filePathes
        |> List.map (Image.importFromTxt >> (fun x -> 
            let res = x |> List.map (fun y -> 
                let classificationResult = clasifyFunction refAvg y
                ( y.Name, classificationResult.ClassName ))
            res
        ))
    result

let clasify2 clasifyFunction filePathes refAvg =
    let result =
        filePathes
        |> List.map (fun path ->
            let importedImages = Image.importFromTxt path
            let res = importedImages |> List.map (fun y -> 
                let classificationResult = clasifyFunction refAvg y
                ( y.Name, classificationResult.ClassName ))
            (path, res)
        )
    result

let clasifyAndPrint (funName, clasifyFunction) filePathes refAvg =
    printfn "result of %s:\n%A" funName (clasify2 clasifyFunction filePathes refAvg)
*)

let clasify (clasifyFunction : list<ClassifiedImage> -> list<UnclassifiedImage> -> list<ClassifiedImage>) refAvgImages images =
    let result = clasifyFunction refAvgImages images
    0

let clasifyAndPrint (funName, clasifyFunction : list<ClassifiedImage> -> list<UnclassifiedImage> -> list<ClassifiedImage>) filePathes refAvgImages =
    //let imagesFromFiles = Image.importFromTxtMulti filePathes
    let classificationResult = 
        filePathes
        |> List.map (fun path -> 
            let importedImages = Image.importFromTxt path
            let result = 
                (clasifyFunction refAvgImages importedImages)
                |> List.map (fun x -> (x.Name, x.ClassName))
            (path, result)
        )
    printfn "result of %s:\n%A" funName classificationResult
    //printfn "result of %s:\n%A" funName (clasify clasifyFunction refAvgImages images)

let getFilePathes (options : CommandLineHelper.CommandLineOptions) =
    options.imagePathes |> List.map (fun x -> options.basePath + "\\" + x)

let getRefFilePathes (options : CommandLineHelper.CommandLineOptions) =
    options.refPathes |> List.map (fun x -> options.basePath + "\\" + x)

[<EntryPoint>]
let main argv =
    (*
    //dotnet run -- -b data\examples\ -r ref1.txt -r ref2.txt -i input1.txt -i rand1.txt -i rand2.txt -m 0

    //let filePathes = ["data\\examples\\input1.txt"]
    //let refFilePathes = ["data\\examples\\ref1.txt"; "data\\examples\\ref2.txt"]

    let filePathes = ["data\\sorts\\input.txt"]
    let refFilePathes = ["data\\sorts\\ref1.txt"; "data\\sorts\\ref2.txt"; "data\\sorts\\ref3.txt"]

    let refImages = Image.importRefFromTxtMulti refFilePathes
    let refAvg = refImages |> List.map Image.calculateReferenceAvg

    let images = Image.importFromTxtMulti filePathes

    //(clasifyAndPrint ("Separating Functions", Classifier.separatingFunctionClassify)) filePathes refAvg
    let result = Classifier.taxonomyMethod refAvg images.Head
    
    0
    *)


    // 1 - dotnet run -- -b data\sorts\ -r ref1.txt -r ref2.txt -r ref3.txt -i input.txt -m 0
    // 2 - dotnet run -- -b data\examples\ -r ref1.txt -r ref2.txt -i input1.txt -i rand1.txt -i rand2.txt -m 0
    let rowOptions = CommandLineHelper.parseCommandLine (argv |> Array.toList)
    match rowOptions with
    | Some options -> 
        let filePathes = getFilePathes options// |> List.map (fun x -> baseExamplesPath + x)
        let refFilePathes = getRefFilePathes options// |> List.map (fun x -> baseExamplesPath + x)

        let refImages = Image.importRefFromTxtMulti refFilePathes
        let refAvg = refImages |> List.map Image.calculateReferenceAvg

        let classificationFunctions = Map([
            (1, ("Reference Equation", Classifier.referenceEquationClassify)); 
            (2, ("Potential Functions", Classifier.potentialFunctionClassify)); 
            (3, ("Separating Functions", Classifier.separatingFunctionClassify));
            (4, ("Bayes Procedures", Classifier.bayesProceduresClassify refImages));
            (5, ("Taxonomy", Classifier.taxonomyClassify));
        ])
        match options.method with
        | 0 ->
            classificationFunctions
            |> Map.map (fun _ x -> (clasifyAndPrint x) filePathes refAvg)
            |> ignore
        | num ->
            let findResult = classificationFunctions.TryFind num
            match findResult with
            | Some existedFunTuple -> 
                (clasifyAndPrint existedFunTuple) filePathes refAvg
            | None -> ()
        0
    | None -> 
        1

    // CLI
    (*
    let rowOptions = CommandLineHelper.parseCommandLine (argv |> Array.toList)
    match rowOptions with
    | Some options -> 
        let filePathes = getFilePathes options// |> List.map (fun x -> baseExamplesPath + x)
        let refFilePathes = getRefFilePathes options// |> List.map (fun x -> baseExamplesPath + x)

        let refImages = Image.importRefFromTxtMulti refFilePathes
        let refAvg = refImages |> List.map Image.calculateReferenceAvg

        let classificationFunctions = Map([
            (1, ("Reference Equation", Classifier.referenceEquationClassify)); 
            (2, ("Potential Functions", Classifier.potentialFunctionClassify)); 
            (3, ("Separating Functions", Classifier.separatingFunctionClassify));
            (4, ("Bayes Procedures", Classifier.bayesProceduresClassify refImages))
        ])
        match options.method with
        | 0 ->
            classificationFunctions
            |> Map.map (fun _ x -> (clasifyAndPrint x) filePathes refAvg)
            |> ignore
        | num ->
            let findResult = classificationFunctions.TryFind num
            match findResult with
            | Some existedFunTuple -> 
                (clasifyAndPrint existedFunTuple) filePathes refAvg
            | None -> ()
        0
    | None -> 
        1
    *)