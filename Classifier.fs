namespace Classifier
open Image

module Classifier =

    let private findSignByName name (signs : list<Sign>) =
        signs |> List.find (fun x -> x.Name = name)

    let private findImageByClassName className (images : list<ClassifiedImage>) =
        images |> List.find (fun x -> x.ClassName = className)

    let private calculateDistanceC (image : ClassifiedImage) refSigns =
        image.Signs
            |> List.sumBy (fun sign -> 
                let ref1Sign = findSignByName sign.Name refSigns
                (ref1Sign.Value - sign.Value) * (ref1Sign.Value - sign.Value))

    let private calculateDistance (image : UnclassifiedImage) refSigns =
        image.Signs
            |> List.sumBy (fun sign -> 
                let ref1Sign = findSignByName sign.Name refSigns
                (ref1Sign.Value - sign.Value) * (ref1Sign.Value - sign.Value))

    let private calculateEuclideanDistanceBySigns (image : UnclassifiedImage) refSigns =
        calculateDistance image refSigns
        |> sqrt

    let private calculateEuclideanDistance (signs1 : list<Sign>) (signs2 : list<Sign>) =
        List.map2 (fun s1 s2 -> 
            //if (s1.Name = s2.Name) then
            (s2.Value - s1.Value) * (s2.Value - s1.Value)
        ) signs1 signs2
        |> List.sum
        |> sqrt

    // Reference equation (nearest neighbours) method
    let referenceEquationSingleClassify (refAvgImages : list<ClassifiedImage>) (image : UnclassifiedImage) =
        let nearestRef =
            refAvgImages
            |> List.map (fun ref -> 
                let euclideanDistance = calculateEuclideanDistanceBySigns image ref.Signs
                (ref, euclideanDistance))
            |> List.minBy (fun (_, euclideanDistance) -> euclideanDistance)
            |> fst
        { ClassName = nearestRef.ClassName; Name = image.Name; Signs = image.Signs}

    let referenceEquationClassify (refAvgImages : list<ClassifiedImage>) (image : list<UnclassifiedImage>) =
        image |> List.map (referenceEquationSingleClassify refAvgImages)

    // Potential functions method
    let private potentialFunction r =
        let f0 = 1.0
        let a = 0.01
        f0 / (1.0 + (a + r) * (a + r))

    let potentialFunctionSingleClassify (refAvgImages : list<ClassifiedImage>) (image : UnclassifiedImage) =
        let nearestRef =
            refAvgImages
            |> List.map (fun ref -> 
                let potentialFunctionValue = 
                    ref.Signs
                    |> calculateEuclideanDistanceBySigns image
                    |> potentialFunction
                (ref, potentialFunctionValue))
            |> List.maxBy (fun (_, potentialFunctionValue) -> potentialFunctionValue)
            |> fst
        { ClassName = nearestRef.ClassName; Name = image.Name; Signs = image.Signs}

    let potentialFunctionClassify (refAvgImages : list<ClassifiedImage>) (image : list<UnclassifiedImage>) =
        image |> List.map (potentialFunctionSingleClassify refAvgImages)

    // Separating functions method
    let private signNames (signs : list<Sign>) =
        signs
        |> List.groupBy (fun x -> x.Name)
        |> List.map (fun (name, sign) -> name)

    let separatingFunctionSingleClassify (refAvgImages : list<ClassifiedImage>) (image : UnclassifiedImage) =
        let unclassified = { ClassName = "Unclassified"; Name = image.Name; Signs = image.Signs; }
        match refAvgImages with
        | [ref1; ref2] ->
            let signNames1 = signNames ref1.Signs
            let signNames2 = signNames ref2.Signs
            let signNamesImg = signNames image.Signs
            if (signNames1 <> signNames2 || signNames1 <> signNamesImg) then unclassified
            else
                let sign1Ref1 = findSignByName signNames1.Head ref1.Signs
                let sign2Ref1 = findSignByName signNames1.Tail.Head ref1.Signs
                let sign1Ref2 = findSignByName signNames1.Head ref2.Signs
                let sign2Ref2 = findSignByName signNames1.Tail.Head ref2.Signs

                let x1 = sign2Ref1.Value
                let x2 = sign2Ref2.Value
                let y1 = sign1Ref1.Value
                let y2 = sign1Ref2.Value

                let k = (y2 - y1) / (x2 - x1)
                let struct (x0, y0) = ((x1 + x2) / 2., (y1 + y2) / 2.) // M

                let imgSignsGroups = 
                    image.Signs
                    |> List.groupBy (fun sign -> sign.Name)
                let signs1 = imgSignsGroups |> List.find (fun (name, signs) -> name = sign1Ref1.Name) |> snd
                let signs2 = imgSignsGroups |> List.find (fun (name, signs) -> name = sign2Ref1.Name) |> snd

                let functionValue = signs1.Head.Value + (signs2.Head.Value - x0) / k - y0

                { ClassName = (if functionValue > 0. then ref2.ClassName else ref1.ClassName); Name = image.Name; Signs = image.Signs; }
        | _ ->
            unclassified

    let separatingFunctionClassify (refAvgImages : list<ClassifiedImage>) (image : list<UnclassifiedImage>) =
        image |> List.map (separatingFunctionSingleClassify refAvgImages)

    // Bayes procedures method
    let bayesProceduresSingleClassify (refImages : list<list<ClassifiedImage>>) (refAvgImages : list<ClassifiedImage>) (image : UnclassifiedImage) =
        let n = refImages |> List.sumBy(fun refList -> refList.Length)
        let distances = refAvgImages |> List.map (fun ref -> (ref, calculateDistance image ref.Signs))

        let dispersions = 
            refImages
            |> List.map (fun refList ->
                let value = 
                    refList |> List.map (fun ref -> 
                        (ref, calculateDistanceC (findImageByClassName ref.ClassName refAvgImages) ref.Signs)
                    ) |> List.sumBy (fun (x, y) -> y)
                (refList.Head.ClassName, value / double refList.Length, refList.Length)
            )

        let f1 = 
            distances |> List.map (fun (ref, distance) -> 
                let _, sigma, ni = (dispersions |> List.find (fun (name, value, ni) -> name = ref.ClassName))
                (ref.ClassName, double 1. / ((pown (2. * 3.14 * sigma) ni / 2.)) * exp (- distance / (2. * sigma)), ni)
            )

        let f = f1 |> List.sumBy (fun (className, f1val, ni) -> double f1val * float ni / float n)

        let p = 
            f1 
            |> List.map (fun (className, f1val, ni) -> (className, float ni / float n * f1val / f)) 
            |> List.maxBy (fun (name, pval) -> pval)
        //printfn "%A" p
        { ClassName = p |> fst; Name = image.Name; Signs = image.Signs; }

    let bayesProceduresClassify (refImages : list<list<ClassifiedImage>>) (refAvgImages : list<ClassifiedImage>) (image : list<UnclassifiedImage>) =
        image |> List.map (bayesProceduresSingleClassify refImages refAvgImages)

    // Taxonomy method
    let rec private taxonomize (images : list<UnclassifiedImage>) (clusterCenters : list<UnclassifiedImage>) (d: float) =
        //debug
        //printfn "d: %f" d
        let distances = 
            images
            |> List.map (fun image ->
                let imageDistancesToEachClusterCenter =
                    clusterCenters
                    |> List.map (fun cluster ->
                        (image, cluster.Name, calculateEuclideanDistance image.Signs cluster.Signs)
                    )
                imageDistancesToEachClusterCenter
            )

        let minDistances = List.map (List.minBy (fun (_, _, distance) -> distance)) distances

        let (candidate, _, maxmin) =
            minDistances
            |> List.maxBy (fun (_, _, distance) -> distance)

        let avgMin = List.averageBy (fun (_, _, d) -> d) minDistances

        if (maxmin > d) then
            taxonomize images ({candidate with Name = sprintf "Cl%d" (clusterCenters.Length + 1)}::clusterCenters) (avgMin / 2.)
        else
            (clusterCenters, distances)

    let taxonomyClassify (refAvgImages : list<ClassifiedImage>) (images : list<UnclassifiedImage>) =
        // unclassidfied max - m1
        let ucMax =
            images
            |> List.maxBy (fun x -> 
                x.Signs
                |> List.maxBy (fun y -> y.Value))
        
        // unclassidfied min - m2
        // the most big distance from m1 (ucMax)
        let ucMin =
            images
            |> List.map (fun x ->
                (x, calculateEuclideanDistance x.Signs ucMax.Signs)
            )
            |> List.maxBy (fun (_, dist) -> dist)
            |> fst

        let d = calculateEuclideanDistance ucMin.Signs ucMax.Signs / 2.

        let (clusterCenters, taxonomizeResult) = taxonomize images [{ucMax with Name = "Cl2"}; {ucMin with Name = "Cl1"}] d

        let refToCluster =
            refAvgImages
            |> List.map (fun ref -> 
                let refDistToEachCluster = clusterCenters |> List.map (fun cc -> (cc.Name, calculateEuclideanDistance ref.Signs cc.Signs))
                ((refDistToEachCluster |> List.minBy (fun (name, distance) -> distance) |> fst), ref)
            )
            |> Map.ofList

        // Map refAvgImages to clusters centers
        let mappedResult =
            taxonomizeResult
            |> List.map (List.minBy (fun (img, clr, dist) -> dist))
            |> List.map (fun (img, clr, _) -> 
                let refImg = refToCluster.[clr]
                { ClassName = refImg.ClassName; Name = img.Name; Signs = img.Signs; }
            )
        //printfn "result:\n%A" mappedResult

        mappedResult