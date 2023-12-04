using System.Text;

record MaybePartNumber(int Start, int End, int Value, bool IsDefinitePartNumber);

class Line {
    private readonly HashSet<int> Symbols = new HashSet<int>();
    private readonly List<MaybePartNumber> PartNumbers = new List<MaybePartNumber>();

    public void AddSymbol(int i) => Symbols.Add(i);

    public void AddPartNumber(MaybePartNumber n) => PartNumbers.Add(n);

    public bool ContainsSymbol(int start, int end) {
        for (var i = start; i <= end; i++) {
            if (Symbols.Contains(i)) {
                return true;
            }
        }
        return false;
    }

    public MaybePartNumber FindAdjacentPartNumber(int i) {
        foreach (var n in PartNumbers) {
            if (n.Start <= i && i <= n.End) {
                return n;
            }
        }
        return null;
    }
}

class MaybePartNumberBuilder {
    private readonly StringBuilder DigitBuilder = new StringBuilder();
    private readonly int StartIndex;
    private readonly bool IsDefinitePartNumber;
    private int EndIndex;

    public MaybePartNumberBuilder(int startIndex, bool isDefinitePartNumber) {
        StartIndex = Math.Max(0, startIndex - 1);
        EndIndex = StartIndex;
        IsDefinitePartNumber = isDefinitePartNumber;
    }

    public void Add(char c) {
        DigitBuilder.Append(c);
        EndIndex++;
    }

    public MaybePartNumber ToMaybePartNumber()
        => new MaybePartNumber(StartIndex, ++EndIndex, int.Parse(DigitBuilder.ToString()), IsDefinitePartNumber);
}

class State {
    private enum Kind { Empty, Symbol, Digit }
    private MaybePartNumberBuilder PartNumberBuilder;
    private Kind Last;

    public int Index { get; private set; }

    public State() { Reset(); }

    public void Reset() {
        PartNumberBuilder = null;
        Last = Kind.Empty;
        Index = -1;
    }

    private MaybePartNumber MakeAndReset() {
        var n = PartNumberBuilder.ToMaybePartNumber();
        PartNumberBuilder = null;
        return n;
    }

    public MaybePartNumber Symbol() {
        Last = Kind.Symbol;
        return PartNumberBuilder != null
            ? MakeAndReset() with { IsDefinitePartNumber = true }
            : null;
    }

    public void Digit(char c) {
        if (PartNumberBuilder == null) {
            PartNumberBuilder = new MaybePartNumberBuilder(Index, Last == Kind.Symbol);
        }
        PartNumberBuilder.Add(c);
        Last = Kind.Digit;
    }

    public MaybePartNumber Empty() {
        Last = Kind.Empty;
        return PartNumberBuilder != null
            ? MakeAndReset()
            : null;
    }

    public void Step() => Index++;
}

public static class GearRatios {
    static int SumParts(IEnumerable<char> schematic) {
        var state = new State();
        var previousLine = new Line();
        var currentLine = new Line();
        var sum = 0;
        void Add(MaybePartNumber n) {
            Console.Error.WriteLine($"Adding definite {n.Value}");
            sum += n.Value;
        }
        void MaybeAdd(MaybePartNumber n) {
            if (n.IsDefinitePartNumber || previousLine.ContainsSymbol(n.Start, n.End)) {
                Add(n);
            } else {
                Console.Error.WriteLine($"Recording possible {n.Value}");
                currentLine.AddPartNumber(n);
            }
        }
        foreach (var c in schematic) {
            state.Step();
            void Empty() {
                if (state.Empty() is MaybePartNumber n) {
                    MaybeAdd(n);
                }
            }
            switch (c) {
                case '\n': {
                    Empty();
                    previousLine = currentLine;
                    currentLine = new Line();
                    state.Reset();
                    continue;
                }
                case '.': {
                    Empty();
                    continue;
                }
                case char _ when Char.IsDigit(c): {
                    state.Digit(c);
                    continue;
                }
                default: {
                    currentLine.AddSymbol(state.Index);
                    if (state.Symbol() is MaybePartNumber n) {
                        MaybeAdd(n);
                    }
                    if (previousLine.FindAdjacentPartNumber(state.Index) is MaybePartNumber m) {
                        Add(m);
                    }
                    continue;
                }
            }
        }
        return sum;
    }

    static IEnumerable<char> GetSchematic(string file) {
        using (var sr = new StreamReader(file)) {
            var buffer = new char[1024];
            var read = 0;
            while ((read = sr.ReadBlock(buffer, 0, buffer.Length)) > 0) {
                for (int i = 0; i < read; i++)
                    yield return buffer[i];
            }
        }
    }

    public static void Main(string[] args) {
        if (args.Length == 1) {
            Console.WriteLine(SumParts(GetSchematic(args[0])));
        }
    }
}
