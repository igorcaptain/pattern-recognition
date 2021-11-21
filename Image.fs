namespace Image
open System.IO

type Sign = {
    Name : string;
    Value : float;
}

type UnclassifiedImage = {
    Name : string;
    Signs : list<Sign>;
}

type ClassifiedImage = {
    ClassName : string;
    Name : string;
    Signs : list<Sign>;
}

type Image = 
    | ClassifiedImage
    | UnclassifiedImage

module Image = 

    let private readMatrixFromTxt filePath =
        filePath 
            |> File.ReadLines
            |> Seq.choose (fun row -> 
                match row with
                | txt when txt.StartsWith("//") -> None
                | "" -> None
                | _ -> row.Split("\t") |> Array.toList |> Some)
            |> Seq.toList

    let importFromTxt filePath =
        let fileLines = readMatrixFromTxt filePath
        let patterns =
            List.transpose fileLines
            |> List.tail
            |> List.map (fun row -> {
                Name = row.Head;
                Signs = row.Tail |> List.mapi (fun i cell ->
                    { Name = fileLines.[i + 1].[0]; Value = float cell })
            })
        patterns

    let importRefFromTxt filePath =
        let fileLines = readMatrixFromTxt filePath
        let patterns =
            fileLines
            |> List.tail
            |> List.transpose
            |> List.tail
            |> List.map (fun row -> {
                ClassName = fileLines.Head.Head;
                Name = row.Head;
                Signs = row.Tail |> List.mapi (fun i cell ->
                    { Name = fileLines.[i + 2].[0]; Value = float cell })
            })
        patterns

    let importFromTxtMulti filePathes =
        filePathes
        |> List.map importFromTxt

    let importRefFromTxtMulti filePathes =
        filePathes
        |> List.map importRefFromTxt

    let calculateReferenceAvg (images : list<ClassifiedImage>) =
        let references = 
            images
            |> List.map (fun x -> x.Signs)
            |> List.transpose
            |> List.map (fun x -> { Name = x.Head.Name; Value = x |> List.averageBy (fun y -> y.Value) })
        { ClassName = images.Head.ClassName; Name = "Reference"; Signs = references }