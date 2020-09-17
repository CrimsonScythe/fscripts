/// (a)
/// ///<summary>
///Funktionen laver en streng der holder hele tabellen. Den bruger if-else sætninger til at udtrække rækker fra tabellen baseret på værdien af n.
///</summary>
///<example>
/// For løkken:
/// <code>
/// for i = 0 to 10 do      
/// s<- s+"\n"+"\n"         
/// if i=0 then
///     s <-s+ "\t"           
///  else
///     s<-s+ (i.ToString()+"\t") 
/// </code>
/// Bygger den første søjle. Første gang den køre indsætter den det første tal eller det første element i søjlen, anden gand det andet osv.
/// "\n laver en ny linje efter hver række"
/// Hvis i er 0, skal 0 ikke tiføjes til s, i stedet skal s få et mellemrum: "\t"
/// Hvis i ikke er 0, skal s få værdien af i,
/// </example>
///<example>
/// Den anden for løkke:
/// <code>
///  for j = 1 to 10 do          
///    if i=0 then
///      s<-s+ (j.ToString()+"\t") 
///    else
///    s<-s+ ((i*j).ToString()+"\t")  
/// </code>
/// Bygger rækkene.
/// Hvis i er 0, får s værdierne 1 til 10.
/// Hvis i ikke er 0, skal tallet fra den første for løkke (i), ganges med 1 til 10. Det ville så give helle rækken for i.
/// </example>
///<remarks>
/// if sætningerne benytter string slicing til at udtrække delle af tabellen der svarer til hvad n værdien er.
/// Hvis n ikke er fra 1 til 10, returnere programmet error beskedet.
///</remarks>
/// 
let mulTable (n:int) : string=
    let mutable s = ""
    for i = 0 to 10 do      
    s<- s+"\n"+"\n"         
    if i=0 then
      s <-s+ "\t"           
    else
      s<-s+ (i.ToString()+"\t") 
    for j = 1 to 10 do          
    if i=0 then
      s<-s+ (j.ToString()+"\t") 
    else
    s<-s+ ((i*j).ToString()+"\t")  
    done
    done
    if n = 1 then              
        s.[0..48]
    elif n =2 then
        s.[0..78]
    elif n = 3 then
        s.[0..109]
    elif n = 4 then
        s.[0..141]
    elif n = 5 then
        s.[0..174] 
    elif n = 6 then
        s.[0..207]
    elif n = 7 then
        s.[0..240]
    elif n = 8 then
        s.[0..273]
    elif n = 9 then
        s.[0..306]
    elif n = 10 then
        s.[0..342]
    else
        "Error"             
    

printfn "%s" (mulTable 1)
printfn "%s" (mulTable 2)
printfn "%s" (mulTable 3)
printfn "%s" (mulTable 10)

/// (b)
/// ///<summary>
///Funktionen bygger en tabel dynamisk baseret på hvad n værdien er (hvor mange antal række tabellen skal have ).
///</summary>
///<example>
/// Forskelen her er at for løkken:
/// <code>
/// for i = 0 to n do
/// s<- s+"\n"+"\n"
///    if i=0 then
///      s <-s+ "\t"
///    else
///      s<-s+ (i.ToString()+"\t")
/// </code>
/// kører op til n hvorimod i mulTable køret den op til 10. På den måde bliver tabellen bygget op dynamisk.
/// </example>
///<example>
/// Den anden for løkke:
/// <code>
///  for j = 1 to 10 do          
///    if i=0 then
///      s<-s+ (j.ToString()+"\t") 
///    else
///    s<-s+ ((i*j).ToString()+"\t")  
/// </code>
/// er den samme som i mulTable siden den bygger rækkene, og der ville altid være præcist 10 elementer i hver række så den kører op til 10.
/// </example>
/// 
let loopMulTable (n:int) : string=
    let mutable s = ""
    for i = 0 to n do       //Forskelen her er at for løkken køre kun op til n, på den måde bliver den bygt dynamisk
    s<- s+"\n"+"\n"
    if i=0 then
      s <-s+ "\t"
    else
      s<-s+ (i.ToString()+"\t")
    for j = 1 to 10 do
    if i=0 then
      s<-s+ (j.ToString()+"\t")
    else
    s<-s+ ((i*j).ToString()+"\t")
    done
    done
    sprintf "%s" s

printfn "%s" (loopMulTable 1)
printfn "%s" (loopMulTable 2)
printfn "%s" (loopMulTable 3)
printfn "%s" (loopMulTable 10)

/// (c)
/// Laver en variabel der har type boolean og giver den en starte værdi af false.
/// Tester om multalbe og looptable giver samme resultat. 
/// Hvis Ja, så får bools værdi true, hvis Nej, så får den værdi false
/// printere tabellen med værdien af bool for hver i
///
/// UPDATE: Outputten svingede i mellem true og false fordi der jeg benyttede streng slicing i mulTable gjorde jeg det ikke præcist nok og den tog derfor noget mellemrum
///  med. Det er nu blevet fixet. 
for i = 1 to 10 do                  
let mutable bools = false                   
if mulTable i = loopMulTable i then          
    bools<-true                             
else    
    bools<-false                            
printfn "%-10s %-10s" (i.ToString()) (bools.ToString())
done 
/// (d)
/// printf "%A" printer resultatet med "", hvorimod printf "%s" printer resultatet uden "".
