using System.Runtime.InteropServices;

namespace PPLD.Decompiler;

struct FuncL
{
    public int Label;
    public int Func;
}

partial class Decompiler
{
    const short LastFunc = unchecked((short)0xfee2);
    const int LastStat = 0x00e2;

    Executable executable;

    // do decompile pass 1
    int akt_stat, akt_proc, uvar_flag = 0, pass = 0;
    int next_label, next_func;
    int func_flag, proc_flag;
    int src_ptr;
    int symbol = 0;
    private bool[] valid_buffer;

    int exp_count, trash_flag;

    Dictionary<int, int> label_used = new();

    Dictionary<int, FuncL> func_used = new();

    Stack<int> label_stack = new();
    Stack<string> string_stack = new();

    public static Decompiler Read(string fileName)
    {
        var result = new Decompiler();

        result.executable = Executable.Read(fileName);

        result.valid_buffer = new bool[result.executable.CodeSize / 2 + 1];
        for (var i = 0; i < result.executable.CodeSize / 2; i++)
        {
            result.valid_buffer[i] = false;
        }

        result.DoPass1();
        result.pass++;
        result.DumpVars();
        result.DoPass2();
        return result;
    }

    void DumpVars()
    {
        int i = 0, cVars = 0, cConst = 0, cFunc = 0, cProc = 0;

        if ((executable.VariableDeclarations[0].VariableType == 0) && (executable.VariableDeclarations[1].VariableType == 0) && (executable.VariableDeclarations[2].VariableType == 0)
           && (executable.VariableDeclarations[3].VariableType == 0) && (executable.VariableDeclarations[4].VariableType == VariableType.Date) && (executable.VariableDeclarations[5].VariableType == VariableType.Integer)
           && (executable.VariableDeclarations[6].VariableType == VariableType.Integer) && (executable.VariableDeclarations[7].VariableType == VariableType.Integer) && (executable.VariableDeclarations[8].VariableType == VariableType.String)
           && (executable.VariableDeclarations[9].VariableType == VariableType.String) && (executable.VariableDeclarations[10].VariableType == VariableType.String) && (executable.VariableDeclarations[11].VariableType == VariableType.String)
           && (executable.VariableDeclarations[12].VariableType == VariableType.String) && (executable.VariableDeclarations[13].VariableType == VariableType.String) && (executable.VariableDeclarations[14].VariableType == VariableType.String)
           && (executable.VariableDeclarations[15].VariableType == 0) && (executable.VariableDeclarations[16].VariableType == 0) && (executable.VariableDeclarations[17].VariableType == 0)
           && (executable.VariableDeclarations[18].VariableType == VariableType.String) && (executable.VariableDeclarations[19].VariableType == VariableType.String) && (executable.VariableDeclarations[20].VariableType == VariableType.String)
           && (executable.VariableDeclarations[21].VariableType == VariableType.String) && (executable.VariableDeclarations[22].VariableType == VariableType.Date) && (executable.VariableDeclarations[20].Dims[0] == 5))
            uvar_flag = 1;

        if ((uvar_flag == 1) && (executable.Version.Major >= 3))
        {
            if (!((executable.VariableDeclarations[23].VariableType == VariableType.Integer) && (executable.VariableDeclarations[23].Dims[0] == 16))) uvar_flag = 0;
        }

        if (symbol == 0)
        {
            Console.WriteLine("\n;Source Code:\n");
            Console.WriteLine(";------------------------------------------------------------------------------\n\n");
        }

        while (i < executable.MaxVar)
        {
            if ((i > 0x16) || (uvar_flag != 1))
            {
                if (executable.VariableDeclarations[i].LFlag == 0)
                {
                    switch (executable.VariableDeclarations[i].VariableType)
                    {
                        case VariableType.Function:
                            if (executable.VariableDeclarations[i].Flag == 1)
                            {
                                executable.VariableDeclarations[i].Number = (++cFunc);
                                executable.VariableDeclarations[executable.VariableDeclarations[i].ReturnVar - 1].Number = cFunc;
                                if (symbol == 0)
                                {
                                    Console.WriteLine("    DECLARE ");
                                    outputFunc(i);
                                    Console.WriteLine();
                                }
                            }
                            break;
                        case VariableType.Method:
                            if (executable.VariableDeclarations[i].Flag == 1)
                            {
                                executable.VariableDeclarations[i].Number = (++cProc);
                                if (symbol == 0)
                                {
                                    Console.WriteLine("    DECLARE ");
                                    outputProc(i);
                                    Console.WriteLine();
                                }
                            }
                            break;
                        default:
                            if (executable.VariableDeclarations[i].Flag == 0) executable.VariableDeclarations[i].Number = (++cConst);
                            else
                            {
                                executable.VariableDeclarations[i].Number = (++cVars);
                                if (symbol == 0)
                                {
                                    Console.Write("    " + TYPE_NAMES[(byte)executable.VariableDeclarations[i].VariableType]);
                                    Console.Write($"  VAR{cVars}");

                                    switch (executable.VariableDeclarations[i].Dim)
                                    {
                                        case 1:
                                            Console.WriteLine($"({executable.VariableDeclarations[i].Dims[0]})");
                                            break;
                                        case 2:
                                            Console.WriteLine($"({executable.VariableDeclarations[i].Dims[0]}, {executable.VariableDeclarations[i].Dims[1]})");
                                            break;
                                        case 3:
                                            Console.WriteLine($"({executable.VariableDeclarations[i].Dims[0]}, {executable.VariableDeclarations[i].Dims[1]}, {executable.VariableDeclarations[i].Dims[2]})");
                                            break;
                                    }
                                    Console.WriteLine();
                                }
                            }
                            break;
                    }
                }
            }
            i++;
        }

        if (symbol == 0) Console.WriteLine("\n;------------------------------------------------------------------------------\n\n");
    }

    void outputFunc(int func)
    {
        int i, j;
        Console.Write($"FUNCTION FUNC{executable.VariableDeclarations[func].Number}(");
        j = executable.VariableDeclarations[func].FirstVar;
        for (i = 0; i < executable.VariableDeclarations[func].Args; i++)
        {
            if (i != 0) Console.Write(",");
            Console.Write(TYPE_NAMES[(byte)executable.VariableDeclarations[j].VariableType]);
            executable.VariableDeclarations[j].Func = func;
            Console.Write($" LOC{executable.VariableDeclarations[j].Number}");
            j++;
        }
        Console.Write($") {TYPE_NAMES[(byte)executable.VariableDeclarations[j].VariableType]}");
    }

    void outputProc(int proc)
    {
        int i, j;
        Console.Write($"PROCEDURE PROC{executable.VariableDeclarations[proc].Number}(");
        j = executable.VariableDeclarations[proc].FirstVar;
        for (i = 0; i < executable.VariableDeclarations[proc].Args; i++)
        {
            if (i != 0) Console.Write(",");
            if (((executable.VariableDeclarations[proc].ReturnVar >> i) & 1) != 0)
            {
                Console.Write("VAR ");
            }
            Console.Write(TYPE_NAMES[(byte)executable.VariableDeclarations[j].VariableType]);
            executable.VariableDeclarations[j].Func = proc;
            Console.Write($" LOC{executable.VariableDeclarations[j].Number}");
            j++;
        }
        Console.Write(")");
    }
    
    void FillValid(int i, int j)
    {
        for (var k = i; k < j; k++) valid_buffer[k] = true;
    }

    void funcin(int label, int func)
    {
        func_used[label] = new FuncL
        {
            Func = func,
            Label = func_used.Count
        };
    }

    void funcout(int label)
    {
        if (func_used.TryGetValue(label, out var func))
        {
            if (func_flag == 1)
            {
                Console.WriteLine("    ENDFUNC\n");
            }
            if (proc_flag == 1)
            {
                Console.WriteLine("    ENDPROC\n");
            }
            func_flag = 0; proc_flag = 0;

            Console.WriteLine();

            if (executable.VariableDeclarations[func.Func].VariableType == VariableType.Function)
            {
                outputFunc(func.Func);
                func_flag = 1;
            }
            else
            {
                outputProc(func.Func);
                proc_flag = 1;
            }

            Console.WriteLine();

            DumpLocs(func.Func);
            if (symbol == 0)
                Console.WriteLine();
        }
    }

    void DumpLocs(int func)
    {
        int i, MaxVar;
        int j = 0;
        // StackPtr = 0;
        if (executable.VariableDeclarations[func].VariableType == VariableType.Function)
        {
            i = executable.VariableDeclarations[func].ReturnVar;
            MaxVar = executable.VariableDeclarations[func].ReturnVar + executable.VariableDeclarations[func].TotalVar;
        }
        else
        {
            i = executable.VariableDeclarations[func].FirstVar + executable.VariableDeclarations[func].Args;
            MaxVar = executable.VariableDeclarations[func].FirstVar + executable.VariableDeclarations[func].TotalVar; //+1;
        }

        while (i < MaxVar)
        {
            if (executable.VariableDeclarations[i].Flag == 1)
            {
                executable.VariableDeclarations[i].Func = func;
                if (symbol == 0)
                {
                    Console.WriteLine();
                    Console.Write("    " + TYPE_NAMES[(byte)executable.VariableDeclarations[i].VariableType]);
                    Console.Write($" LOC{executable.VariableDeclarations[i].Number}");
                    switch (executable.VariableDeclarations[i].Dim)
                    {
                        case 1: Console.Write($"({executable.VariableDeclarations[i].Dims[0]}) "); break;
                        case 2:
                            Console.Write($"({executable.VariableDeclarations[i].Dims[0]},{executable.VariableDeclarations[i].Dims[1]}) "); break;
                        case 3:
                            Console.Write($"({executable.VariableDeclarations[i].Dims[0]},{executable.VariableDeclarations[i].Dims[1]},{executable.VariableDeclarations[i].Dims[2]}) "); break;
                    }
                }
            }
            i++; j++;
        }
        if (j > 0) Console.WriteLine();
    }

    void labelin(int label)
    {
        this.label_used[label] = label_used.Count + 1;
    }

    int labelnr(int label)
    {
        if (label_used.TryGetValue(label, out var number))
            return number;
        return -1;
    }

    void labelout(int label)
    {
        if (label_used.TryGetValue(label, out var number))
        {
            Console.WriteLine();
            Console.WriteLine($":LABEL{number}");
        }
    }

    void pushlabel(int label)
    {
        label_stack.Push(label);
    }

    int poplabel()
    {
        if (label_stack.Count == 0)
            return -2;
        return label_stack.Pop();
    }

    void pushstr(string str)
    {
        exp_count++;
        string_stack.Push(str);
    }

    string popstr()
    {
        if (string_stack.Count == 0)
            return "";
        exp_count--;
        return string_stack.Pop();
    }

    string popstrip()
    {
        return stripper(popstr());
    }

    string stripper(string str)
    {
        /*
        while (str.StartsWith("(") && str.EndsWith(")"))
        {
            str = str.Substring(1, str.Length - 2);
        }*/
        return str; 
    }

    string ConstName(int[] vars, string[] names)
    {
        string endptr;
        int i = 0;

        var TempStr = popstrip();
        if (int.TryParse(TempStr, out var val))
        {
            while ((vars[i] != -1) && (vars[i] != val))
                i++;
            if (vars[i] != -1)
                return names[i];
        }
        return (TempStr);
    }

    string TransExp(int curExpr)
    {
        switch (akt_stat)
        {
            case 0x00c:
                if (curExpr != 2) return (popstrip());
                else return (ConstName(CONSTANT_CONFERENCE_OFFSETS, CONSTANT_CONFERENCE_NAMES));
            case 0x00d:
                if (curExpr != 2) return (popstrip());
                else return (ConstName(CONSTANT_CONFERENCE_OFFSETS, CONSTANT_CONFERENCE_NAMES));
            case 0x00e:
                if (curExpr != 2) return (popstrip());
                else return (ConstName(CONSTANT_NAMES_OFFSETS, CONSTANT_NAMES_DISPLAY));
            case 0x010:
                if (curExpr == 3) return (ConstName(CONSTANT_FACCESS_OFFSETS, CONSTANT_FACCESS_NAMES));
                else if (curExpr == 4)
                    return (ConstName(CONSTANT_OPENFLAGS_OFFSETS, CONSTANT_OPENFLAGS_NAMES));
                else return (popstrip());
            case 0x011:
                if (curExpr == 3) return (ConstName(CONSTANT_FACCESS_OFFSETS, CONSTANT_FACCESS_NAMES));
                else if (curExpr == 4)
                    return (ConstName(CONSTANT_OPENFLAGS_OFFSETS, CONSTANT_OPENFLAGS_NAMES));
                else return (popstrip());
            case 0x012:
                if (curExpr == 3) return (ConstName(CONSTANT_FACCESS_OFFSETS, CONSTANT_FACCESS_NAMES));
                else if (curExpr == 4)
                    return (ConstName(CONSTANT_OPENFLAGS_OFFSETS, CONSTANT_OPENFLAGS_NAMES));
                else return (popstrip());
            case 0x018: return (ConstName(CONSTANT_LINECOUNT_OFFSETS, CONSTANT_LINECOUNT_NAMES));
            case 0x022:
                if (curExpr != 6) return (popstrip());
                else return (ConstName(CONSTANT_1_OFFSETS, CONSTANT_1_NAMES));
            case 0x02b:
                if (curExpr != 5) return (popstrip());
                else return (ConstName(CONSTANT_1_OFFSETS, CONSTANT_1_NAMES));
            case 0x039:
                if (curExpr != 2) return (popstrip());
                else return (ConstName(CONSTANT_1_OFFSETS, CONSTANT_1_NAMES));
            case 0x070:
                if (curExpr != 3) return (popstrip());
                else return (ConstName(CONSTANT_SEEK_OFFSETS, CONSTANT_SEEK_NAMES));
            case 0x0cd:
            case 0x0ce:
                if (curExpr != 1) return (popstrip());
                else return (ConstName(CONSTANT_2_OFFSETS, CONSTANT_2_NAMES));
            default: return (popstrip());
        }
    }

    string varout(int varNr)
    {
        int i = 0, j = 0;
        //	char TempStr[1000]; char TempStr2[1000];
        varNr--;
        if (((executable.Version.Major < 3) && (varNr < 0x17) && (uvar_flag == 1)) || ((executable.Version.Major >= 3) && (varNr < 0x18) && (uvar_flag == 1)))
        {
            return USER_VARIABLE_NAMES[varNr + 1];
        }
        if (executable.VariableDeclarations[varNr].Flag == 1)
        {
            switch (executable.VariableDeclarations[varNr].VariableType)
            {
                case VariableType.Function:
                    return $"FUNC{executable.VariableDeclarations[varNr].Number}";
                case VariableType.Method:
                    return $"PROC{executable.VariableDeclarations[varNr].Number}";
                default:
                    if (executable.VariableDeclarations[varNr].FFlag == 1)
                        return $"FUNC{executable.VariableDeclarations[varNr].Number}";
                    if (executable.VariableDeclarations[varNr].LFlag == 1)
                        return $"LOC{executable.VariableDeclarations[varNr].Number}";
                    return $"VAR{executable.VariableDeclarations[varNr].Number}";
            }
        }
        switch (executable.VariableDeclarations[varNr].VariableType)
        {
            case VariableType.Boolean:
                return executable.VariableDeclarations[varNr].Content != 0 ? "TRUE" : "FALSE";
            case VariableType.Date:
                return executable.VariableDeclarations[varNr].Content.ToString(); 
            case VariableType.Money:
                return $"${executable.VariableDeclarations[varNr].Content / 10.0}";
            case VariableType.String:
                return '"' + executable.VariableDeclarations[varNr].StringValue + '"';
            case VariableType.Time: return ((long)executable.VariableDeclarations[varNr].Content).ToString();
            case VariableType.Double: return ((double)executable.VariableDeclarations[varNr].Content).ToString();
            default: return ((long)executable.VariableDeclarations[varNr].Content).ToString();
        }
    }

    int fnktout(int func)
    {
        int i = 0;

        switch (FUNCTION_SIGNATURE_TABLE[func])
        {
            case 0x10:
                if (exp_count < 1) return (-1);
                pushstr("(" + EXPR_NAMES[func] + popstr() + ")");
                return 0;
            case 0x11:
                if (exp_count < 2) return (-1);
                var tempStr2 = popstr() + ")";
                var tempStr = "(" + popstr();
                pushstr(tempStr + EXPR_NAMES[func] + tempStr2);
                return 0;
            default:
                if (exp_count < FUNCTION_SIGNATURE_TABLE[func]) return (-1);
                string TempStr2 = ")";
                while (FUNCTION_SIGNATURE_TABLE[func] > i++)
                {
                    var TempStr = popstr();
                    if (i != 1) TempStr += ",";
                    TempStr += TempStr2;
                    TempStr2 = TempStr;
                }
                pushstr(EXPR_NAMES[func] + "(" + TempStr2);
                return 0;
        }
    }

    int dimexpr(int dims)
    {
        int AktDim;
        int TrashFunc;
        int ExpTemp;

        ExpTemp = exp_count;
        AktDim = 0;

        if (pass == 1)
        {
            pushstr(popstr() + "(");
        }

        while (dims != AktDim++)
        {
            exp_count = 0; TrashFunc = 0;
            src_ptr++;
            if ((AktDim != 1) && (pass == 1))
            {
                pushstr(popstr() + ",");
            }

            while (executable. SourceBuffer[src_ptr] != 0)
            {
                if (executable. SourceBuffer[src_ptr] <= executable.MaxVar)
                {
                    if (executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].VariableType == VariableType.Function)
                    {
                        pushlabel(executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Start);
                        executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Flag = 1;
                        funcin(executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Start, executable. SourceBuffer[src_ptr] - 1);
                        if (pass == 1)
                        {
                            var tmp2 = varout(executable. SourceBuffer[src_ptr]);
                            pushstr(popstr() + tmp2 + "(");
                        }
                        src_ptr++;
                        if (getexpr(executable.VariableDeclarations[executable. SourceBuffer[src_ptr - 1] - 1].Args, 1) != 0)
                            return (1);
                        src_ptr--;
                        if (pass == 1)
                        {
                            pushstr(popstr() + ")");
                        }
                    }
                    else
                    {
                        if (pass == 1) pushstr(varout(executable. SourceBuffer[src_ptr]));
                        if (executable. SourceBuffer[++src_ptr] != 0)
                        {
                            executable.VariableDeclarations[executable. SourceBuffer[src_ptr - 1] - 1].Flag = 1;
                            if (dimexpr(executable. SourceBuffer[src_ptr]) != 0)
                            {
                                return 1;
                            }
                        }
                    }
                }
                else
                {
                    var x = -executable. SourceBuffer[src_ptr] - 1;
                    if ((executable. SourceBuffer[src_ptr] < LastFunc) || (x >= 0 && FUNCTION_SIGNATURE_TABLE[x] == 0xaa))
                    {
                        throw new InvalidOperationException($"Error: Unknown function {executable. SourceBuffer[src_ptr]}");
                    }

                    if (pass == 1)
                    {
                        if (fnktout(-executable. SourceBuffer[src_ptr] - 1) != 0)
                        {
                            TrashFunc = executable. SourceBuffer[src_ptr];
                            trash_flag = 1;
                        }
                        else if ((TrashFunc != 0) && (fnktout(-TrashFunc - 1) == 0)) TrashFunc = 0;
                    }
                }
                src_ptr++;
            }

            if (pass == 1)
            {
                var tempstr2 = popstr();
                pushstr(popstr() + tempstr2);
            }
        }

        if (pass == 1)
        {
            pushstr(popstr() + ")");
        }

        exp_count = ExpTemp;
        return 0;
    }

    int getexpr(int maxExpr, int rec)
    {
        int AktExp = 0;
        int i;
        int ExpTemp;
        short TrashFunc;

        ExpTemp = exp_count;
        src_ptr++;
        while ((maxExpr & 0x0ff) != AktExp++)
        {
            exp_count = 0; TrashFunc = 0;
            if ((AktExp != 1) && (pass == 1))
            {
                if ((akt_stat == 8) && (rec == 0)) pushstr(popstr() + "=");
                else pushstr(popstr() + ",");
            }

            while (executable. SourceBuffer[src_ptr] != 0)
            {
                var b = executable. SourceBuffer[src_ptr] <= executable.MaxVar;
                if (executable. SourceBuffer[src_ptr] >= 0 && executable. SourceBuffer[src_ptr] <= executable.MaxVar)
                {
                    if ((maxExpr / 256 == AktExp) || (maxExpr / 256 == 0x0f))
                    {
                        executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Flag = 1;
                        if (pass == 1) pushstr(varout(executable. SourceBuffer[src_ptr]));
                        if (executable. SourceBuffer[++src_ptr] != 0)
                        {
                            if (dimexpr(executable. SourceBuffer[src_ptr]) != 0) return (1);
                            if (pass == 1)
                            {
                                var TempStr2 = popstr();
                                var TempStr = popstr();
                                pushstr(TempStr + TempStr2);
                            }
                        }
                    }
                    else
                    {
                        if (akt_stat == 0xa8)
                        {
                            if (((executable.VariableDeclarations[akt_proc].ReturnVar >> (AktExp - 1)) & 1) != 0)
                            {
                                executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Flag = 1;
                            }
                        }
                        if (executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].VariableType == VariableType.Function)
                        {
                            pushlabel(executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Start);
                            executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Flag = 1;
                            funcin(executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Start, executable. SourceBuffer[src_ptr] - 1);
                            if (pass == 1)
                            {
                                var tmp2 = varout(executable. SourceBuffer[src_ptr]);
                                pushstr(popstr() + tmp2 + "(");
                            }
                            src_ptr++;
                            if (getexpr(executable.VariableDeclarations[executable. SourceBuffer[src_ptr - 1] - 1].Args, 1) != 0) return (1);
                            if (pass == 1)
                            {
                                pushstr(popstr() + ")");
                            }
                        }
                        else
                        {
                            if (pass == 1) pushstr(varout(executable. SourceBuffer[src_ptr]));
                            if (executable. SourceBuffer[++src_ptr] != 0)
                            {
                                executable.VariableDeclarations[executable. SourceBuffer[src_ptr - 1] - 1].Flag = 1;
                                if (dimexpr(executable. SourceBuffer[src_ptr]) != 0) return (1);
                                if (pass == 1)
                                {
                                    /*								strcpy(TempStr2,popstr());
                                                                    strcpy(TempStr,popstr());
                                                                    strcat(TempStr,TempStr2);
                                                                    pushstr(TempStr); */
                                }
                            }
                            src_ptr++;
                        }
                    }
                }
                else
                {
                    if ((executable. SourceBuffer[src_ptr] < LastFunc) || (FUNCTION_SIGNATURE_TABLE[-executable. SourceBuffer[src_ptr] - 1] == 0xaa))
                    {
                        throw new InvalidOperationException("Error: Unknown function " + executable. SourceBuffer[src_ptr]);
                    }

                    if (pass == 1)
                    {
                        if (fnktout(-executable. SourceBuffer[src_ptr] - 1) != 0)
                        {
                            TrashFunc = executable. SourceBuffer[src_ptr];
                            trash_flag = 1;
                        }
                        else if ((TrashFunc != 0) && (fnktout(-TrashFunc - 1) == 0)) TrashFunc = 0;
                    }
                    src_ptr++;
                }
            }

            src_ptr++;
            if (pass == 1)
            {
                var tmp2 = TransExp(AktExp);
                pushstr(popstr() + tmp2);
            }
        }

        exp_count = ExpTemp + 1;
        return (0);
    }

    int setifptr(int i)
    {
        try
        {
            if ((executable. SourceBuffer[executable. SourceBuffer[src_ptr] / 2 - 2] == 0x0007) &&
                (executable. SourceBuffer[executable. SourceBuffer[src_ptr] / 2 - 1] / 2 == i))
            {
                executable. SourceBuffer[i] = 0;
                i = executable. SourceBuffer[src_ptr] / 2 - 2;
            }
            else i = -5;
            return (i);
        } catch
        {
            return i - 5;
        }
    }

    int DoPass1()
    {
        int prevStat, lastPoint;
        int ifPtr;

        akt_stat = 0;
        next_label = 0;
        next_func = 0;
        src_ptr = 0;
        ifPtr = -5;
        do
        {
            lastPoint = src_ptr;

        More:
            if ((src_ptr >= executable.CodeSize / 2) || valid_buffer[src_ptr])
            {
                FillValid(lastPoint, src_ptr);
                goto Trap;
            }

            prevStat = akt_stat;
            if (src_ptr == ifPtr) src_ptr += 2;

            akt_stat = executable. SourceBuffer[src_ptr];
            if ((akt_stat > LastStat) || (akt_stat < STATEMENT_SIGNATURE_TABLE.Length && STATEMENT_SIGNATURE_TABLE[akt_stat] == 0xaa))
            {
                throw new InvalidOperationException($"Error: Unknown statement {akt_stat}.");
            }

            switch (STATEMENT_SIGNATURE_TABLE[akt_stat])
            {
                case 0xf6:
                    src_ptr += 2;
                    akt_proc = executable. SourceBuffer[src_ptr - 1] - 1;
                    executable.VariableDeclarations[akt_proc].Flag = 1;
                    funcin(executable.VariableDeclarations[akt_proc].Start, akt_proc);
                    pushlabel(executable.VariableDeclarations[akt_proc].Start);
                    if (getexpr(executable.VariableDeclarations[akt_proc].Args, 0) != 0) goto Trap; break;
                case 0xf7:
                    if (getexpr(0x01, 0) != 0) goto Trap;
                    executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Flag = 1;
                    if (getexpr(0x01, 0) != 0) goto Trap; break;
                case 0xf8:
                    if (getexpr(0x03, 0) != 0) goto Trap;
                    executable.VariableDeclarations[executable. SourceBuffer[src_ptr] - 1].Flag = 1;
                    src_ptr++; break;
                case 0xf9:
                    executable.VariableDeclarations[executable. SourceBuffer[src_ptr + 1] - 1].Flag = 1;
                    executable.VariableDeclarations[executable. SourceBuffer[src_ptr + 2] - 1].Flag = 1;
                    src_ptr += 3; break;
                case 0xfe: if (getexpr(executable. SourceBuffer[++src_ptr], 0) != 0) goto Trap; break;
                case 0xfa:
                    executable.VariableDeclarations[executable. SourceBuffer[src_ptr + 2] - 1].Flag = 1;
                    src_ptr += 2;
                    if (getexpr(executable. SourceBuffer[src_ptr - 1] - 1, 0) != 0) goto Trap; break;
                case 0xfc: if (getexpr(executable. SourceBuffer[++src_ptr] | 0xf00, 0) != 0) goto Trap; break;
                case 0xfd:
                    src_ptr++;
                    labelin(executable. SourceBuffer[src_ptr]);
                    src_ptr++; break;
                case 0xff:
                    ifPtr = src_ptr;
                    if (getexpr(0x01, 0) != 0) goto Trap;
                    ifPtr = setifptr(ifPtr);
                    src_ptr++; break;
                default:
                    if (getexpr((STATEMENT_SIGNATURE_TABLE[akt_stat] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[akt_stat] & 0x0f), 0) != 0) goto Trap;
                    break;
            }
            switch (akt_stat)
            {
                case 0x0001: //END
                case 0x002a: //RETURN
                case 0x00a9: //ENDPROC
                case 0x00ab: //ENDFUNC
                case 0x003a:
                    FillValid(lastPoint, src_ptr); //STOP
                    lastPoint = src_ptr;
                    if (prevStat == 0x000b) goto More;
                    break;
                case 0x0029:
                    FillValid(lastPoint, src_ptr); //GOSUB
                    lastPoint = src_ptr;
                    pushlabel(executable. SourceBuffer[src_ptr - 1]);
                    goto More;
                case 0x0007:
                    FillValid(lastPoint, src_ptr); //GOTO
                    lastPoint = src_ptr;
                    pushlabel(executable. SourceBuffer[src_ptr - 1]);
                    if (prevStat == 0x000b) goto More;
                    break;
                case 0x000b:
                    FillValid(lastPoint, src_ptr); //IF
                    lastPoint = src_ptr;
                    pushlabel(executable. SourceBuffer[src_ptr - 1]);
                    goto More;
                default: goto More;
            }
        Trap:;
            src_ptr = poplabel() / 2;
        } while ((src_ptr != -1) && (src_ptr <= (executable.CodeSize / 2)));
        return 0;
    }

    int DoPass2()
    {
        int prevStat, ifPtr;

        akt_stat = -1;
        next_label = 0;
        next_func = 0;
        src_ptr = 0;
        trash_flag = 0;
        ifPtr = -1;

        while (src_ptr < executable.CodeSize / 2)
        {
            prevStat = akt_stat;
            // StackPtr = 0;
            if (src_ptr == ifPtr) src_ptr += 2;

            funcout(src_ptr * 2);
            labelout(src_ptr * 2);
            if (!valid_buffer[src_ptr])
            {
                src_ptr++;
                continue;
            }

            akt_stat = executable. SourceBuffer[src_ptr];
           // Console.WriteLine(SrcPtr +":"+ AktStat);
            if ((akt_stat > LastStat) || (STATEMENT_SIGNATURE_TABLE[akt_stat] == 0xaa))
            {
                throw new InvalidOperationException($"Error: Unknown statement {akt_stat}, aborting…");
            }

            if (prevStat < 0 || STATEMENT_SIGNATURE_TABLE[prevStat] != 0xff)
            {
                Console.Write("    ");
            }
            Console.Write(STATEMENT_NAMES[akt_stat]);

            if (akt_stat != 0xa8) Console.Write(" ");
            
            switch (STATEMENT_SIGNATURE_TABLE[akt_stat])
            {
                case 0xf6:
                    src_ptr += 2;
                    akt_proc = executable. SourceBuffer[src_ptr - 1] - 1;
                    Console.Write(executable.VariableDeclarations[akt_proc].Number + "(");
                    if (getexpr(executable.VariableDeclarations[executable. SourceBuffer[src_ptr - 1] - 1].Args, 0) != 0) return (1);
                    Console.Write(popstr());
                    Console.Write(")");
                    break;
                case 0xf7:
                    if (getexpr(0x01, 0) != 0) return (1);
                    Console.Write("," + varout(executable. SourceBuffer[src_ptr]) + ",");
                    if (getexpr(0x01, 0) != 0) return (1);
                    Console.Write(popstr()); break;
                case 0xf8:
                    if (getexpr(0x03, 0) != 0) return (1);
                    Console.Write(popstr());
                    Console.Write("," + varout(executable. SourceBuffer[src_ptr]));
                    src_ptr++; break;
                case 0xf9:
                    Console.Write(varout(executable. SourceBuffer[src_ptr + 1]));
                    Console.Write(",");
                    Console.Write(varout(executable. SourceBuffer[src_ptr + 2]));
                    src_ptr += 3; break;
                case 0xfe:
                    if (getexpr(executable. SourceBuffer[++src_ptr], 0) != 0) return (1);
                    Console.Write(popstr()); break;
                case 0xfa:
                    Console.Write("%s,", varout(executable. SourceBuffer[src_ptr + 2]));
                    src_ptr += 2;
                    if (getexpr(executable. SourceBuffer[src_ptr - 1] - 1, 0) != 0) return (1);
                    Console.Write(popstr()); break;
                case 0xfc:
                    if (getexpr(executable. SourceBuffer[++src_ptr] | 0xf00, 0) != 0) return (1);
                    Console.Write(popstr()); break;
                case 0xfd:
                    src_ptr++;
                    Console.Write("LABEL" + labelnr(executable. SourceBuffer[src_ptr]));
                    src_ptr++; break;
                case 0xff:
                    ifPtr = src_ptr;
                    Console.Write("(");
                    if (getexpr(0x001, 0) != 0) return (1);
                    Console.Write(popstr());
                    Console.Write( ") ");
                    ifPtr = setifptr(ifPtr);
                    src_ptr++; break;
                default:
                    if (getexpr((STATEMENT_SIGNATURE_TABLE[akt_stat] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[akt_stat] & 0x0f), 0) != 0) return (1);
                    Console.Write(popstr());
                    break;
            }

            if (STATEMENT_SIGNATURE_TABLE[akt_stat] != 0xff)
            {
                if (trash_flag != 0)
                {
                    Console.Write("   ; PPLC bug detected");
                    trash_flag = 0;
                }
                Console.WriteLine();
            }

            switch (akt_stat)
            {
                case 0x0001: //END
                case 0x002a: //RETURN
                case 0x003a:
                    Console.WriteLine(); //STOP
                    break;
                case 0x00a9: //ENDPROC
                case 0x00ab:
                    func_flag = 0; proc_flag = 0; //ENDFUNC
                    Console.WriteLine();
                    break;
            }

        }
        labelout(src_ptr * 2);
        return (0);
    }

}
