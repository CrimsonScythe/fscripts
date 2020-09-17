/// (a)
///<summary>
///Laver en funktion som tager en int som input og returnere en int.
///</summary>
///<param name="i">
///En lokal variabel der er mutable fordi værdien skal ændres nede i while løkket.
///</param>
///<param name="s">
///En lokal variabel der er mutable fordi værdien skal ændres nede i while løkket
///</param>
///<returns>
///s, summen af tal fra 1 til n
///</returns>
///<remarks>
///While løkket kørere helle vejen op til n, og tilføjer i værdien til s hver gang således at når den er færdi med at køre, 
///svarer s til summen af tal fra 1 til n.
///</remarks>
let sum (n:int) : int=      
    let mutable i = 0      
    let mutable s = 0       
    while i<n do             
        i<-i+1              
        s<-s+i
    s                       

printfn "%d" (sum 10) 

/// (b)
///<summary>
///Laver en funktion simpleSum som tager en int som input og retunere en int.
///</summary>
///<returns>
/// summen af tal fra 1 til n
///</returns>
/// <remarks>
/// (n*(n+1))/2
/// er en matematisk udtryk der giver summen af tal i en aritmetisk række.
/// </remarks>
let simpleSum (n : int) : int =     
    (n*(n+1))/2                     

printfn "%d" (simpleSum 10) 

/// (c)
///<summary>
///Funktionen beder brugeren at indtaste et tal vha. tastaturet. Den bruger så resultatet som input til sum() og simpleSum()
///</summary>
///<param name="d">
///Resultatet fra funktionen System.Console.ReadLine() har type: string. Her bliver den oversat til int, og bundet med en local variabel d.
///</param>
///<remarks>
///d bliver brugt som input argumentet til sum() og simplesum(). Resultatet bliver oversat til
/// en streng vha. ToString() således at '+' operatoren kan benyttes når printfn "%s" er brugt.
///</remarks>

System.Console.WriteLine("tæst ind et tal og tryk ENTER...")            
let d = (int (System.Console.ReadLine()))                               
                                                                        
printfn "%s" ("sum returnere: "+((sum d).ToString()))                   
printfn "%s" ("simpleSum returnere: "+((simpleSum d).ToString()))       

/// (d)
///<summary>
///-10 giver en mellemrum i mellem hver element
/// For løkken printer 10 rækker som inholder resultatet af sum og simpleSum.
///</summary>

printfn "%-10s %-10s %-10s" "n" "sum" "simpleSum n"                    

for i = 1 to 10 do                                                     
printfn "%-10d %-10d %-10d" i (sum i) (simpleSum i)
done

/// (e)
/// Den støreste n værdi er 2147483646. For at beregne for større værdier skal return typene og parameter typen skiftes til int64. Det kunne også skiftes til uint32 og 
/// uint64.
/// Nedenfor har jeg brugt int64 og sættede L foran numrene for at indikere at de har typen int64.
/// Funktionerne krasher hvis man sætter et decimaltal eller en float/double som input.
let sum2 (n:int64) : int64=
    let mutable i = 0L
    let mutable s = 0L
    while i<n do
        i<-i+1L
        s<-s+i
    s

    
printfn "%d" (sum2 2147483647L)