using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using TypeInfo = Microsoft.CodeAnalysis.TypeInfo;

namespace SneakyToString
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class SneakyToStringAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "SneakyToString";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Usage";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
            context.RegisterSyntaxNodeAction(AnalyzeInterpolation, SyntaxKind.Interpolation);
        }

        private void AnalyzeInterpolation(SyntaxNodeAnalysisContext context)
        {
            InterpolationSyntax syntax = (InterpolationSyntax) context.Node;

            var type = context.SemanticModel.GetTypeInfo(syntax.Expression);
            
            if(IsItAlreadyObject(type, context))
                return;

            if(HasToStringOverride(type, context))
                return;

            context.ReportDiagnostic(Diagnostic.Create(Rule, syntax.GetLocation()));

        }
        private void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
        {

            InvocationExpressionSyntax expressionSyntax = (InvocationExpressionSyntax)context.Node;

            if(!(expressionSyntax.Expression is MemberAccessExpressionSyntax expression))
               return;

            if(expression.Name.Identifier.ValueText != "ToString")
                return;


            TypeInfo type = context.SemanticModel.GetTypeInfo(expression.Expression);

            if(IsItAlreadyObject(type, context))
                return;

            if(HasToStringOverride(type, context))
                return;

            Location loc = expression.GetLocation();
            context.ReportDiagnostic(Diagnostic.Create(Rule, loc));
        }

        private Boolean IsItAlreadyObject(TypeInfo type, SyntaxNodeAnalysisContext context)
        {
            if (type.Type.Equals(context.SemanticModel.Compilation.GetTypeByMetadataName(typeof(Object).FullName)))
                return true;

            return false;
        }
        private Boolean HasToStringOverride(TypeInfo type, SyntaxNodeAnalysisContext context)
        {
            var toStringMethod = GetTypeToStringMethods(type);

            if (toStringMethod.Any())
            {
                if (!IsToStringIsShadow(toStringMethod, context))
                    return true;
            }

            return false;
        }
        private List<IMethodSymbol> GetTypeToStringMethods(TypeInfo type)
        {
            return type
                .Type
                .GetMembers("ToString")
                .Where(k => k is IMethodSymbol)
                .OfType<IMethodSymbol>()
                .Where(k => k.IsOverride && k.Kind == SymbolKind.Method)
                .ToList();
        }

        private Boolean IsToStringIsShadow(List<IMethodSymbol> methods, SyntaxNodeAnalysisContext context)
        {
            while (methods.Any(k => k.IsOverride))
            {
                if (methods
                    .Any(k => k.OverriddenMethod.ReceiverType
                        .Equals(context.SemanticModel.Compilation.GetTypeByMetadataName(typeof(Object).FullName))))
                    return false;

                methods = methods.Select(k => k.OverriddenMethod).ToList();
            }

            return true;
        }
        private void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            var localDeclaration = (LocalDeclarationStatementSyntax) context.Node;

            // Ensure that all variables in the local declaration have initializers that
            // are assigned with constant values.
            foreach (var variable in localDeclaration.Declaration.Variables)
            {
                var initializer = variable.Initializer;
                if (initializer == null)
                {
                    return;
                }

                var constantValue = context.SemanticModel.GetConstantValue(initializer.Value);
                if (!constantValue.HasValue)
                {
                    return;
                }
            }

            // Perform data flow analysis on the local declaration.
            var dataFlowAnalysis = context.SemanticModel.AnalyzeDataFlow(localDeclaration);

            foreach (var variable in localDeclaration.Declaration.Variables)
            {
                // Retrieve the local symbol for each variable in the local declaration
                // and ensure that it is not written outside of the data flow analysis region.
                var variableSymbol = context.SemanticModel.GetDeclaredSymbol(variable);
                if (dataFlowAnalysis.WrittenOutside.Contains(variableSymbol))
                {
                    return;
                }
            }

            context.ReportDiagnostic(Diagnostic.Create(Rule, context.Node.GetLocation()));
        }
    }
}
