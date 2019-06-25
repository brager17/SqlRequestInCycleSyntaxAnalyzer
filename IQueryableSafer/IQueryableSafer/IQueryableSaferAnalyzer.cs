using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Linq.Expressions;
using System.Runtime.InteropServices.ComTypes;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
namespace IQueryableSafer
{

    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class IQueryableSaferAnalyzer : DiagnosticAnalyzer
    {
        #region descriptions

        public const string DiagnosticId = "IQueryableSafer";

        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle),
            Resources.ResourceManager, typeof(Resources));

        private static readonly LocalizableString MessageFormat =
            new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager,
                typeof(Resources));

        private static readonly LocalizableString Description =
            new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager,
                typeof(Resources));

        private const string Category = "SQL Request In Cycle";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat,
         Category, DiagnosticSeverity.Error, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);
        #endregion

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(NodeAction, SyntaxKind.ForStatement, SyntaxKind.ForStatement, SyntaxKind.InvocationExpression);
        }

        private static string[] InvocationExpression { get; } = GetQueryableMaterializeMethodsName().ToArray();


        private void NodeAction(SyntaxNodeAnalysisContext context)
        {
            if (new[] { SyntaxKind.ForStatement, SyntaxKind.ForEachStatement }.Contains(context.Node.Kind()))
            {
                var SqlRequestInCycle = context.Node.DescendantNodes().OfType<InvocationExpressionSyntax>()
                    .Select(x => AnalyzeInvocationExpressionInCycle(x, context.SemanticModel))
                    .Where(x => x != null)
                    .ToList();

                SqlRequestInCycle.ForEach(x => context.ReportDiagnostic(Diagnostic.Create(Rule, x)));
            }

            if (context.Node.Kind() == SyntaxKind.InvocationExpression)
            {
                var locationsIfQueryableMaterialize =
                    AnalyzeLinqCycle(context.Node as InvocationExpressionSyntax, context.SemanticModel);

                locationsIfQueryableMaterialize.Where(x => x != null).ToList()
                    .ForEach(x => { context.ReportDiagnostic(Diagnostic.Create(Rule, x)); });
            }

        }

        private static IEnumerable<Location> AnalyzeLinqCycle(InvocationExpressionSyntax ies, SemanticModel sm)
        {
            new[] { 1, 2 }.AsQueryable();

            var childNodes = ies.ChildNodes();
            if (childNodes.Count() != 2)
                return null;

            var left = ies.ChildNodes().First();
            var right = ies.ChildNodes().Last();

            var leftSymbol = sm.GetSymbolInfo(left);

            if (leftSymbol.Symbol is IMethodSymbol methodSymbol && right is ArgumentListSyntax argumentList)
            {
                if (methodSymbol.ContainingType.Name != "Enumerable")
                {
                    return Enumerable.Empty<Location>();
                }

                var invocationsExpressionsInLinqMethod = argumentList.DescendantNodes().OfType<InvocationExpressionSyntax>();

                var GetLocationIfHasQueryMaterialize =
                    invocationsExpressionsInLinqMethod.Select(x => AnalyzeInvocationExpressionInCycle(x, sm));

                return GetLocationIfHasQueryMaterialize;
            }

            return Enumerable.Empty<Location>();
        }
        private static Location AnalyzeInvocationExpressionInCycle(InvocationExpressionSyntax ies, SemanticModel sm)
        {
            var memberAccessExpressionSyntaxs = ies.ChildNodes().OfType<MemberAccessExpressionSyntax>();

            if (memberAccessExpressionSyntaxs.Count() != 1)
            {
                return Exit();
            }

            var simpleMemberAccessExpression = memberAccessExpressionSyntaxs.Single();

            var partMembers = simpleMemberAccessExpression.ChildNodes();

            if (partMembers.Count() != 2)
            {
                return Exit();
            }

            var right = partMembers.Last();

            if (!(right is IdentifierNameSyntax identifier))
            {
                return Exit();
            }

            var rightSymbol = sm.GetSymbolInfo(right);

            if (!(rightSymbol.Symbol is IMethodSymbol methodSymbol))
            {
                return null;
            }

            if (!InvocationExpression.Contains(identifier.Identifier.ValueText))
            {
                return Exit();
            }

            var linqMethodSymbol = sm.GetSymbolInfo(right);

            var left = partMembers.First();

            var leftPartSimpleMemberAccessExpressionSymbol = sm.GetSymbolInfo(left);


            if (linqMethodSymbol.Symbol is IMethodSymbol rigthMethodSymbol == false)
            {
                return Exit();
            }

            string leftTypeName = null;

            if (leftPartSimpleMemberAccessExpressionSymbol.Symbol is ILocalSymbol localSymbol)
            {
                return ReturnIfTypeIsIQueryable(localSymbol.Type);
            }

            if (leftPartSimpleMemberAccessExpressionSymbol.Symbol is IMethodSymbol leftMethodSymbol)
            {
                return ReturnIfTypeIsIQueryable(leftMethodSymbol.ReturnType);
            }

            return Exit();
            Location Exit() => null;

            Location ReturnIfTypeIsIQueryable(ITypeSymbol typeInfo) =>
                typeInfo.Name == "IQueryable" ? right.GetLocation() : null;
        }


        const string programText1 =
            @"using System;
            using System.Collections.Generic;
            using System.Text;
                          using System.Linq;

            namespace HelloWorld
            {
                class Program
                {
                    static void Main(string[] args)
                    {
                    var t = Enumerable.Range(1,100);
                    var tt = Queryable.AsQueryable();
                    }
                }
            }";

        private static IEnumerable<string> GetQueryableMaterializeMethodsName()
        {
            var stree = CSharpSyntaxTree.ParseText(programText1);
            var compilationRoot = stree.GetCompilationUnitRoot();

            // <Snippet2>
            var compilation1 = CSharpCompilation.Create("abc")
                .AddReferences(MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location))
                .AddReferences(MetadataReference.CreateFromFile(typeof(Queryable).Assembly.Location))
                .AddSyntaxTrees(stree);

            var semanticModel = compilation1.GetSemanticModel(stree);

            var descendantNodes = compilationRoot
                .DescendantNodes();

            var getQueryable = GetIdentifierName(descendantNodes, "Queryable");

            var queryableSymbo = semanticModel.GetSymbolInfo(getQueryable);

            IEnumerable<string> queryableSymbol = null;
            //todo дописать
            var queryableMethodsNames = Enumerable.Empty<string>();
            //
            if (queryableSymbo.Symbol is INamedTypeSymbol enumerableNamedTypeSymbol)
            {
                queryableSymbol = GetMethods(enumerableNamedTypeSymbol)
                    .Where(x => x.Parameters.Any() && x.Parameters.First().Type.Name == "IQueryable" &&
                                x.ReturnType.Name != "IQueryable" && x.ReturnType.Name != "IOrderedQueryable")
                    .Select(x => x.Name)
                    .Distinct();
            }

            return queryableSymbol.Concat(new[] { "ToList" });
        }


        private static List<IMethodSymbol> GetMethods(INamedTypeSymbol namedTypeSymbol)
        {
            return namedTypeSymbol.GetMembers()
                .Where(x => x.Kind == SymbolKind.Method)
                .Cast<IMethodSymbol>()
                .ToList();
        }

        private static IdentifierNameSyntax GetIdentifierName(IEnumerable<SyntaxNode> descendantNodes, string className)
        {
            var identifierNameSyntaxs = descendantNodes
                .OfType<IdentifierNameSyntax>();
            return identifierNameSyntaxs
                .Single(x => x.Identifier.ValueText == className);
        }

    }
}