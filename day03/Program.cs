using System.Text;

enum Kind { Empty, Digit, Symbol, Star }

static class KindUtil {
    public static Kind LeastUpperBound(this Kind a, Kind b) => (Kind)Math.Max((int)a, (int)b);

    public static bool IsSymbol(this Kind a) => a >= Kind.Symbol;
}

record MaybePartNumber(int Start, int End, int Value, Kind Kind);

class Line {
    private readonly Dictionary<int, Kind> Symbols = new Dictionary<int, Kind>();
    private readonly List<MaybePartNumber> PartNumbers = new List<MaybePartNumber>();

    public void AddSymbol(int i, char c) => AddSymbol(i, c == '*' ? Kind.Star : Kind.Symbol);

    public void AddSymbol(int i, Kind kind) => Symbols[i] = kind;

    public void AddPartNumber(MaybePartNumber n) => PartNumbers.Add(n);

    public Kind ContainsSymbol(int start, int end) {
        for (var i = start; i <= end; i++) {
            if (Symbols.TryGetValue(i, out var kind)) {
                return kind;
            }
        }
        return Kind.Empty;
    }

    public List<MaybePartNumber> FindAdjacentPartNumbers(int i) {
        var ns = new List<MaybePartNumber>();
        foreach (var n in PartNumbers) {
            if (n.Start <= i && i <= n.End) {
                ns.Add(n);
            }
        }
        return ns;
    }
}

class MaybePartNumberBuilder {
    private readonly StringBuilder DigitBuilder = new StringBuilder();
    private readonly int StartIndex;
    private readonly Kind Kind;

    public MaybePartNumberBuilder(int startIndex, Kind kind) {
        StartIndex = Math.Max(0, startIndex - 1);
        Kind = kind;
    }

    public void Add(char c) {
        DigitBuilder.Append(c);
    }

    public MaybePartNumber ToMaybePartNumber(int index)
        => new MaybePartNumber(StartIndex, index, int.Parse(DigitBuilder.ToString()), Kind);
}

class State {
    private MaybePartNumberBuilder PartNumberBuilder;
    private Kind Last;
    public int Row {get; private set; }
    public int Index { get; private set; }

    public State() { Reset(); }

    public void Reset() {
        PartNumberBuilder = null;
        Last = Kind.Empty;
        Index = -1;
        Row++;
    }

    private MaybePartNumber MakeAndReset() {
        var n = PartNumberBuilder.ToMaybePartNumber(Index);
        PartNumberBuilder = null;
        return n;
    }

    public MaybePartNumber Symbol(char c) {
        Last = c == '*' ? Kind.Star : Kind.Symbol;
        return PartNumberBuilder != null
            ? MakeAndReset() with { Kind = Last }
            : null;
    }

    public void Digit(char c) {
        if (PartNumberBuilder == null) {
            PartNumberBuilder = new MaybePartNumberBuilder(Index, Last);
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
    static (long, long) SumParts(IEnumerable<char> schematic) {
        var state = new State();
        var previousLine = new Line();
        var currentLine = new Line();
        var gears = new Dictionary<(int, int), List<MaybePartNumber>>();
        var sum = 0L;
        void Add(MaybePartNumber n) {
            sum += n.Value;
        }
        void MaybeAdd(MaybePartNumber n) {
            var kind = n.Kind.LeastUpperBound(previousLine.ContainsSymbol(n.Start, n.End));
            if (kind.IsSymbol()) {
                Add(n);
            } else {
                currentLine.AddPartNumber(n);
            }
            if (kind == Kind.Star) {
                var key = (state.Index, state.Row);
                if (gears.TryGetValue(key, out var ns)) {
                    ns.Add(n);
                } else {
                    gears[key] = new List<MaybePartNumber> { n };
                }
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
                    currentLine.AddSymbol(state.Index, c);
                    if (state.Symbol(c) is MaybePartNumber n) {
                        MaybeAdd(n);
                    }
                    foreach (var m in previousLine.FindAdjacentPartNumbers(state.Index)) {
                        // TODO: We want to record the symbol kind here, too.
                        Add(m);
                    }
                    continue;
                }
            }
        }
        return (sum,
                gears
                .Values
                .Where(ns => ns.Count == 2)
                .Select(ns => ns
                        .Select(n => n.Value)
                        .Aggregate((a, b) => a * b))
                .Sum());
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
