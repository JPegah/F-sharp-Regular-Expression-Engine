open System.Collections.Generic;;
open System

type State = {
    mutable num:int;
    mutable accept :bool;
    mutable dir: Dictionary<string,State>;
    mutable epsilons: List<State>;
}

type NFA = {
   // states : List<State>;
    mutable starts : State;
    mutable accepts : List<State>;
}

let rec findEps (current:State) (index:int) (epsList:List<State>) = 
    if current.num = 0 then false
    else
        //printfn "First Path"
        if current.accept = true then true
        else
            //printfn "size of EPS is %d in %d" epsList.Count current.num
            if index < epsList.Count then
                current.num <- 0
                //printfn "secondPath"
                if findEps (epsList.Item(index)) (0) (epsList.Item(index).epsilons) = true then  
                    current.num <- 5 
                    true
                else 
                    current.num <- 5
                    let d = findEps (current) (index + 1) (epsList)
                //    printfn "This is correct path %b"
                    
                    d
                 
                    
            else
                false
                
                     
                

// check if phrase is in the NFA
let rec find  (currentState:State)  (phrase:string) = 
    if phrase.Length <= 0 then 
        //printfn "end is %d" currentState.num
        
        if currentState.accept = true then true
        else if findEps (currentState) (0) (currentState.epsilons) = true then true
            else false
    
    else    
        
        let a = phrase.Chars(0).ToString();
    //    printfn "Hello %s in %s" a phrase
        let t = currentState.dir.ContainsKey(a);
        if t = true then find (currentState.dir.Item(a)) (phrase.Substring(1))
        else 
    //       printfn "Bye %s in %s" a phrase 
                     
           myFor (currentState) (phrase) (0)
            
          
           
 and myFor   (current:State) (phrase: string) (i:int) = 
 //   printfn "Start of %d %d" i current.num
    
 //   printfn "size of epsilons: %d" i
    if current.epsilons.Count <= i then 
   //     printfn "End of %d" i
        false
    else 
        if find  (current.epsilons.Item(i)) (phrase) = true then true
        else  myFor (current) (phrase) (i + 1)    
        

// create single charachter NFA
let createNFA (tr: string) = 
    let dic1 = new Dictionary<string, State>();
    let dic2 = new Dictionary<string, State>();
    let epsilon1 = new List<State>();
    let epsilon2 = new List<State>(); 
    let state1 = {num = 6; accept = false; dir = dic1; epsilons = epsilon1};
    let state2 = {num = 7; accept = false; dir = dic2; epsilons = epsilon2};
    
    dic1.Add(tr, state2);  
    let accept = new List<State>(); 
    accept.Add(state2); 
    let myNFA = { starts = state1; accepts = accept};    
    
    myNFA   
        

let rec AddToEps (accList:List<State>) (acc:State) (index:int) = 
    if accList.Count <= index then true
    else 
    //    printfn "added epsilon --------"
        accList.Item(index).epsilons.Add(acc);
        AddToEps (accList) (acc) (index + 1);

let orNFA (a:NFA) (b:NFA) = 
    let dic1 = new Dictionary<string, State>();
    let dic2 = new Dictionary<string, State>();
    let epsilon1 = new List<State>();
    let epsilon2 = new List<State>(); 
    let acc = new List<State>();
    epsilon1.Add(a.starts);
    epsilon1.Add(b.starts);
    let state1 = {num = 200; accept = false; dir = dic1; epsilons = epsilon1};
    let state2 = {num = 54; accept = false; dir = dic2; epsilons = epsilon2};
    acc.Add(state2);
  
    let myNFA = {starts = state1; accepts = acc};
    let tmp = AddToEps (b.accepts) (state2) (0); 
    let tmp1 = AddToEps (a.accepts) (state2) (0);
    myNFA.accepts.Add(state2);
   /// printfn "size of accept list is : %d" b.accepts.Count
   // printfn "epslong list inside or is of silze %d" (a.accepts.Item(0).epsilons).Count
   // printfn "epslong list inside or is of silze %d" (b.accepts.Item(0).epsilons).Count
   
    myNFA

let KleeneNFA (a:NFA)  = 
    let dic1 = new Dictionary<string, State>();
    let dic2 = new Dictionary<string, State>();
    let epsilon1 = new List<State>();
    let epsilon2 = new List<State>(); 
    let acc = new List<State>();
    epsilon1.Add(a.starts);
    
    let state1 = {num = 200; accept = false; dir = dic1; epsilons = epsilon1};
    let state2 = {num = 54; accept = false; dir = dic2; epsilons = epsilon2};
    acc.Add(state2);
    epsilon1.Add(state2);  
    let myNFA = {starts = state1; accepts = acc};
    let tmp = AddToEps (a.accepts) (a.starts) (0); 
    let tmp1 = AddToEps (a.accepts) (state2) (0);
    myNFA.accepts.Add(state2);
  //  printfn "size of accept list is : %d" b.accepts.Count
   // printfn "epslong list inside or is of silze %d" (a.accepts.Item(0).epsilons).Count
   // printfn "epslong list inside or is of silze %d" (b.accepts.Item(0).epsilons).Count
   
    myNFA
    
    
let KleenePlusNFA (a:NFA)  = 
    let dic1 = new Dictionary<string, State>();
    let dic2 = new Dictionary<string, State>();
    let epsilon1 = new List<State>();
    let epsilon2 = new List<State>(); 
    let acc = new List<State>();
    epsilon1.Add(a.starts);
    
    let state1 = {num = 200; accept = false; dir = dic1; epsilons = epsilon1};
    let state2 = {num = 54; accept = false; dir = dic2; epsilons = epsilon2};
    acc.Add(state2);
 ///   epsilon1.Add(state2);  
    let myNFA = {starts = state1; accepts = acc};
    let tmp = AddToEps (a.accepts) (a.starts) (0); 
    let tmp1 = AddToEps (a.accepts) (state2) (0);
    myNFA.accepts.Add(state2);
  //  printfn "size of accept list is : %d" b.accepts.Count
  //  printfn "epslong list inside or is of silze %d" (a.accepts.Item(0).epsilons).Count
   // printfn "epslong list inside or is of silze %d" (b.accepts.Item(0).epsilons).Count
   
    myNFA


let QuestionNFA (a:NFA)  = 
    let dic1 = new Dictionary<string, State>();
    let dic2 = new Dictionary<string, State>();
    let epsilon1 = new List<State>();
    let epsilon2 = new List<State>(); 
    let acc = new List<State>();
    epsilon1.Add(a.starts);
    
    let state1 = {num = 200; accept = false; dir = dic1; epsilons = epsilon1};
    let state2 = {num = 54; accept = false; dir = dic2; epsilons = epsilon2};
    acc.Add(state2);
    epsilon1.Add(state2);  
    let myNFA = {starts = state1; accepts = acc};
   // let tmp = AddToEps (a.accepts) (a.starts) (0); 
    let tmp1 = AddToEps (a.accepts) (state2) (0);
    myNFA.accepts.Add(state2);
  //  printfn "size of accept list is : %d" b.accepts.Count
   // printfn "epslong list inside or is of silze %d" (a.accepts.Item(0).epsilons).Count
   // printfn "epslong list inside or is of silze %d" (b.accepts.Item(0).epsilons).Count
   
    myNFA
    
    
let concatNFA (a:NFA) (b:NFA) = 
    let dic1 = new Dictionary<string, State>();
    let dic2 = new Dictionary<string, State>();
    let epsilon1 = new List<State>();
    let epsilon2 = new List<State>(); 
    let acc = new List<State>();
    epsilon1.Add(a.starts);
    
    let state1 = {num = 200; accept = false; dir = dic1; epsilons = epsilon1};
    let state2 = {num = 54; accept = false; dir = dic2; epsilons = epsilon2};
    acc.Add(state2);
 ///   epsilon1.Add(state2);  
    let myNFA = {starts = state1; accepts = acc};
    let tmp = AddToEps (a.accepts) (b.starts) (0); 
    let tmp1 = AddToEps (b.accepts) (state2) (0);
    myNFA.accepts.Add(state2);
  //  printfn "size of accept list is : %d" b.accepts.Count
   // printfn "epslong list inside or is of silze %d" (a.accepts.Item(0).epsilons).Count
   // printfn "epslong list inside or is of silze %d" (b.accepts.Item(0).epsilons).Count
   
    myNFA
    

// detect outer or
let isSpecialChar (phrase:string) curser =
    if curser = 0 then false
    else if phrase.Chars(curser - 1) = '\\' then true
    else false



let rec findOr (phrase:string) (openP:int) (curser:int) = 
    if curser >= phrase.Length then -1
    else if phrase.Chars(curser) = '\\' then
        findOr phrase openP (curser + 1)
    else
        if phrase.Chars(curser) = '|' && (isSpecialChar phrase curser) = true then
            if openP = 0 then curser
            else findOr (phrase) (openP) (curser + 1)
        else        
             if phrase.Chars(curser) = ')' && (isSpecialChar phrase curser) = true then findOr (phrase) (openP - 1) (curser + 1)
             else
                if phrase.Chars(curser) = '(' && (isSpecialChar phrase curser) = true then findOr (phrase) (openP + 1) (curser + 1)
                else findOr (phrase) (openP) (curser + 1)

// detect outer concat
let rec findConcat (phrase:string) openP curser = 
    if curser >= phrase.Length then -1
    else 
            if phrase.Chars(curser) = '\\' || curser = 0 then
                findConcat phrase openP (curser + 1)
            else 
                if (isSpecialChar phrase curser) = false then
                    if openP = 0 then curser
                    else findConcat phrase openP (curser + 1)
                else 
                    if phrase.Chars(curser) = '(' then
                        if curser >= 2 && ((isSpecialChar phrase (curser - 2)) = false || ((isSpecialChar phrase (curser - 2)) = true && phrase.Chars(curser - 2) <> '|')) then 
                            if openP = 0 then curser - 1
                            else findConcat phrase (openP + 1) (curser + 1) 
                        else findConcat phrase (openP + 1) (curser + 1)
                    else
                        if phrase.Chars(curser) = ')' then findConcat phrase (openP - 1) (curser + 1)
                        else findConcat phrase openP (curser + 1)


             

             

let rec orPhrase (phrase:string) = 
    let ind = findOr (phrase) (0) (0)
    if ind = -1 then concatPhrase (phrase)
    else 
        let leftPhrase = phrase.[0..(ind - 2)]
        let rightPhrase = phrase.Substring(ind + 1)
        let leftNFA = orPhrase (leftPhrase)
        let rightNFA = orPhrase (rightPhrase)
        orNFA (leftNFA) (rightNFA)
    

and concatPhrase (phrase:string) = 
    let ind = findConcat (phrase) (0) (0)
    if ind = -1 then powerPhrase (phrase)
    else 
        let leftPhrase = phrase.[0..ind - 1]
        let rightPhrase = phrase.Substring(ind)
        let leftNFA = orPhrase (leftPhrase)
        let rightNFA = orPhrase (rightPhrase)
        concatNFA (leftNFA) (rightNFA)

and powerPhrase (phrase:string) = 
    if phrase.Length <= 1 then createNFA (phrase)
    else 
        let lastChar = phrase.Chars(phrase.Length - 1)
        if isSpecialChar phrase (phrase.Length - 1) = false then
            let subNFA = orPhrase phrase
            subNFA
        else // here is dealing with ) and ^* & ^+ and ^?
            if lastChar = ')' then
                let lastInd = phrase.Length - 3
                let subNFA = orPhrase (phrase.[2..lastInd]);
                subNFA
            else  // here is dealing with ^* ^+ and ^?
                let lastInd = phrase.Length - 3
                if lastChar = '*' then
                    let subNFA = orPhrase (phrase.[0..lastInd])
                    let complete = KleeneNFA (subNFA)
                    complete
                else if lastChar = '+' then
                    let subNFA = orPhrase(phrase.[0..lastInd])
                    let complete = KleenePlusNFA (subNFA)
                    complete
                else //if lastChar = '?' then
                    let subNFA = orPhrase (phrase.[0..lastInd])
                    let complete = QuestionNFA (subNFA)
                    complete
                //else
                    
//copmpare phrase with regular expression
let myMatch (phrase:string) (mregix:string) =   
    let result = orPhrase mregix
    result.accepts.Item(0).accept <- true;
    let matchRes = find (result.starts) phrase
    if matchRes = true then "phrase matched to the regular expression"
    else  "phrase didn't match to the regular expression"       


//***************Enter your inputs here****************
let regix = "t\+\(d\)\?"
let phrase = "tttttttt"
let result = myMatch phrase regix


//print the result
printfn "%s" result