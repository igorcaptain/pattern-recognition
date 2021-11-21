module CommandLineHelper

    type CommandLineOptions = {
        refPathes : list<string>;
        imagePathes : list<string>;
        basePath : string;
        method : int;
    }

    let rec private parseCommandLineRec args optionsSoFar =
        match args with
        | [] -> 
            optionsSoFar
        | "-r"::xs when xs.Length > 0 ->
            let newOptionsSoFar = { optionsSoFar with refPathes = xs.Head :: optionsSoFar.refPathes }
            parseCommandLineRec xs.Tail newOptionsSoFar
        | "-i"::xs when xs.Length > 0 ->
            let newOptionsSoFar = { optionsSoFar with imagePathes = xs.Head :: optionsSoFar.imagePathes }
            parseCommandLineRec xs.Tail newOptionsSoFar
        | "-m"::xs when xs.Length > 0 ->
            let newOptionsSoFar = { optionsSoFar with method = int xs.Head }
            parseCommandLineRec xs.Tail newOptionsSoFar
        | "-b"::xs when xs.Length > 0 ->
            let newOptionsSoFar = { optionsSoFar with basePath = xs.Head }
            parseCommandLineRec xs.Tail newOptionsSoFar
        | x::xs ->
            eprintf "Option '%s' is unrecognized\n" x
            parseCommandLineRec xs optionsSoFar

    let parseCommandLine args =
        let defaultOptions = {
            refPathes = [];
            imagePathes = [];
            basePath = "";
            method = 0
        }

        let result = 
            match (parseCommandLineRec args defaultOptions) with
            | options when options.imagePathes.Length > 0 && options.refPathes.Length > 0 ->
                Some options
            | _ -> None

        result