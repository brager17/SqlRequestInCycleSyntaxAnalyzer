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
            context.RegisterSyntaxNodeAction(NodeAction, SyntaxKind.ForStatement, SyntaxKind.ForStatement);
        }

        private static string[] InvocationExpression = {
            "First", "FirstOrDefault", "ToList"
        };

        private void NodeAction(SyntaxNodeAnalysisContext context)
        {
            if (new[] { SyntaxKind.ForStatement, SyntaxKind.ForEachStatement }.Contains(context.Node.Kind()))
            {
                var SqlRequestInCycle = context.Node.DescendantNodes().OfType<InvocationExpressionSyntax>()
                    .Select(x => AnalyzeInvocationExpression(x, context.SemanticModel))
                    .Where(x => x != null)
                    .ToList();

                SqlRequestInCycle.ForEach(x => context.ReportDiagnostic(Diagnostic.Create(Rule, x)));
            }

        }

        private static Location AnalyzeInvocationExpression(InvocationExpressionSyntax ies, SemanticModel sm)
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

            var rigth = partMembers.Last();

            if (!(rigth is IdentifierNameSyntax identifier))
            {
                return Exit();
            }

            if (!InvocationExpression.Contains(identifier.Identifier.ValueText))
            {
                return Exit();
            }

            var linqMethodSymbol = sm.GetSymbolInfo(rigth);

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
                typeInfo.Name == "IQueryable" ? rigth.GetLocation() : null;
        }
    }
}