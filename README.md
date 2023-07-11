# Type inference engine for λ cube systems

Small learning project. Infers types for an arbitrary term, written with all the necessary type annotations 
(Church style typing), using one of 8 systems of [λ cube](https://en.wikipedia.org/wiki/Lambda_cube).

## Usage
You can call either `infer` or `infer0` inside `ghci`.
First argument is `rules` which are defined for each system as follows:

- `rulesS` is for `λ→`
- `rulesF` is for `λ2`
- `rulesO` is for `λ⍹`
- `rulesP` is for `λP`
- `rulesPF` is for `λP2`
- `rulesPO` is for `λP⍹`
- `rulesFO` is for `λω`
- `rulesPFO` is for `λPω`

For `infer` you need to also specify the context which is a list
of terms that will be available to the inferable term.
`infer0` assumes the context is empty.

Finally, you provide the term which type you want to infer.
Terms are defined using the [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index).
You specify term using `lE` and `pE`, for example
`lE "x" SomeType SomeTerm` means `λx: SomeType. SomeExpr`

`Pi` is universal quantifier, used to 'lift' terms to the 'realm' of types
when using systems with dependant types. Also `Pi (EDecl _ a) b` corresponds to
an 'arrow' type `a -> b`

Examples of usage:
```Haskell
$ ghci implementation.hs
ghci> :l examples.hs
ghci> infer rulesS [EDecl "a" Ast] (lE "x" (Idx 0) $ Idx 0)
Just (Pi (EDecl "b" (Idx 0)) (Idx 1))
ghci> infer0 rulesPFO $ lE "a" Ast $ tBool
Just (Pi (EDecl "a" Ast) Ast)
ghci> infer0 rulesF $ lE "a" Ast $ lE "x" (Idx 0) $ lE "y" (Idx 1) $ Idx 1
Just (Pi (EDecl "a" Ast) (Pi (EDecl "x" (Idx 0)) (Pi (EDecl "y" (Idx 1)) (Idx 2))))
```
