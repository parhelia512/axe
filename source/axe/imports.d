module axe.imports;

import axe.structs;
import axe.lexer;
import axe.parser;
import std.file;
import std.path;
import std.stdio;
import std.algorithm;
import std.string;
import std.array;
import std.exception;

/**
 * Process use statements and merge imported ASTs
 */
ASTNode processImports(ASTNode ast, string baseDir, bool isAxec)
{
    auto programNode = cast(ProgramNode) ast;
    if (programNode is null)
        return ast;
    
    ASTNode[] newChildren;
    string[string] importedFunctions; // Maps original name -> prefixed name
    
    foreach (child; programNode.children)
    {
        if (child.nodeType == "Use")
        {
            auto useNode = cast(UseNode) child;
            string modulePath = buildPath(baseDir, useNode.moduleName ~ ".axe");
            
            if (!exists(modulePath))
            {
                throw new Exception("Module not found: " ~ modulePath);
            }
            
            // Parse the imported module
            string importSource = readText(modulePath);
            auto importTokens = lex(importSource);
            auto importAst = parse(importTokens, isAxec);
            auto importProgram = cast(ProgramNode) importAst;
            
            // Extract requested functions and add them with prefixed names
            foreach (importChild; importProgram.children)
            {
                if (importChild.nodeType == "Function")
                {
                    auto funcNode = cast(FunctionNode) importChild;
                    if (useNode.imports.canFind(funcNode.name))
                    {
                        // Prefix function name to avoid collisions
                        string prefixedName = useNode.moduleName ~ "_" ~ funcNode.name;
                        importedFunctions[funcNode.name] = prefixedName;
                        
                        // Create a new function node with prefixed name
                        auto newFunc = new FunctionNode(prefixedName, funcNode.params);
                        newFunc.returnType = funcNode.returnType;
                        newFunc.children = funcNode.children;
                        newChildren ~= newFunc;
                    }
                }
            }
        }
        else
        {
            // Rename function calls to use prefixed names
            renameFunctionCalls(child, importedFunctions);
            newChildren ~= child;
        }
    }
    
    programNode.children = newChildren;
    return programNode;
}

/**
 * Recursively rename function calls to use prefixed names
 */
void renameFunctionCalls(ASTNode node, string[string] nameMap)
{
    if (node.nodeType == "FunctionCall")
    {
        auto callNode = cast(FunctionCallNode) node;
        if (callNode.functionName in nameMap)
        {
            callNode.functionName = nameMap[callNode.functionName];
        }
    }
    
    foreach (child; node.children)
    {
        renameFunctionCalls(child, nameMap);
    }
}
