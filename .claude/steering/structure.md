# Project Structure - Emergent Spacetime Research

## Directory Organization

### Root Level Structure
```
emergent_spacetime/
├── .claude/                    # Claude-specific configurations
│   └── steering/              # Persistent project context
├── papers/                    # Research papers and publications
├── code/                      # Implementation code
├── research_artifacts/        # Supporting materials
├── index.html                 # Project overview webpage
├── arxiv.sty                  # LaTeX style for papers
├── README.md                  # Main project documentation
└── emergent_spacetime_artifact.md  # Research summary
```

### Papers Organization
```
papers/
├── quantum-gravity/           # Core quantum gravity research
├── information-theory/        # Information-theoretic methods
├── cosmology/                # Cosmological applications
├── foundations/              # Foundational quantum mechanics
└── consciousness-studies/    # Information approaches to cognition

Each subdirectory contains:
├── src/                      # LaTeX source files (.tex, .bib)
└── compiled/                 # Generated PDF documents
```

### Code Organization
```
code/
├── haskell/                  # Functional implementations
│   ├── README.md            # Haskell-specific documentation
│   ├── *_haskell.hs        # Individual physics modules
│   ├── examples/            # Example usage code
│   └── [project-dirs]/      # Structured Haskell projects
└── python/                   # Numerical simulations
    ├── README.md            # Python-specific documentation
    └── *_code.py            # Physics simulation scripts
```

## Naming Conventions

### File Naming

#### Papers
- **LaTeX sources**: `descriptive_name_paper.tex` or `descriptive_name_treatise.tex`
- **PDFs**: Same as source with `.pdf` extension
- **Bibliography**: `[paper_name]Notes.bib` for paper-specific references
- Use underscores, not hyphens, for consistency

#### Code Files
- **Haskell modules**: `concept_name_haskell.hs` or `ConceptName.hs` for modules
- **Python scripts**: `concept_name_code.py`
- **Project files**: `.cabal`, `package.yaml`, `stack.yaml` for Haskell projects

### Module Organization

#### Haskell Projects
```
project-name/
├── src/                      # Source code
│   └── Namespace/           # Module namespace
│       ├── Core.hs         # Core types and functions
│       ├── [Feature].hs    # Feature-specific modules
│       └── Examples.hs     # Usage examples
├── app/                     # Executable entry points
│   └── Main.hs
├── test/                    # Test suite
│   ├── Spec.hs            # Test runner
│   └── [Module]Spec.hs    # Module-specific tests
├── README.md               # Project documentation
└── [project].cabal         # Build configuration
```

## Code Style Guidelines

### Haskell Conventions
- **Module names**: CapitalizedWords (e.g., `InformationGeometry`)
- **Functions**: camelCase (e.g., `computeEntanglement`)
- **Types**: CapitalizedWords (e.g., `QuantumState`)
- **Type parameters**: Single lowercase letters (e.g., `State s a`)
- **Constants**: ALL_CAPS or camelCase depending on context

### Python Conventions
- **Files**: snake_case (e.g., `quantum_gravity_simulation.py`)
- **Functions**: snake_case (e.g., `compute_entanglement`)
- **Classes**: CapitalizedWords (e.g., `QuantumState`)
- **Constants**: ALL_CAPS (e.g., `PLANCK_CONSTANT`)

### LaTeX Conventions
- **Commands**: `\newcommand{\commandName}` with descriptive names
- **Labels**: `{type:descriptive-name}` (e.g., `{eq:einstein-field}`)
- **Sections**: Capitalize major words in titles
- **Citations**: Use BibTeX keys like `Author2020` or `AuthorEtAl2020`

## Documentation Standards

### Code Documentation

#### Haskell
```haskell
-- | Brief description of the module
-- 
-- Longer description explaining the physics concepts
-- and mathematical foundations.
module ModuleName where

-- | Function description with physics context
-- 
-- >>> example usage
-- expected output
functionName :: Type -> Type
```

#### Python
```python
"""
Module description explaining physics concepts.

This module implements [specific physics topic] based on
[theoretical foundation].
"""

def function_name(param):
    """
    Brief description.
    
    Args:
        param: Description with units if applicable
        
    Returns:
        Description of return value with physics meaning
        
    Notes:
        Mathematical details or physics context
    """
```

### Paper Documentation
- Clear abstract summarizing key results
- Introduction providing physics context
- Methods section with mathematical rigor
- Results with clear physical interpretation
- Comprehensive references to prior work

## Testing Structure

### Unit Tests
- Test pure mathematical functions
- Verify conservation laws
- Check dimensional consistency
- Validate against known solutions

### Integration Tests
- End-to-end physics simulations
- Consistency between different methods
- Performance benchmarks

### Property Tests
- Invariants (unitarity, conservation)
- Symmetries (gauge, Lorentz)
- Mathematical properties (associativity, etc.)

## Build and Deployment

### Haskell Projects
- Use Stack or Cabal consistently within a project
- Specify exact dependency versions
- Include build instructions in README

### Python Scripts
- Requirements.txt or environment.yml
- Virtual environment recommended
- Version specifications for reproducibility

### Papers
- Makefile for complex LaTeX builds
- Include all necessary style files
- Document compilation process

## Version Control Patterns

### Branch Strategy
- `main`: Stable, tested code and completed papers
- `feature/*`: New physics implementations
- `paper/*`: Draft papers in progress
- `fix/*`: Bug fixes and corrections

### Commit Messages
```
feat: implement tensor network contraction algorithm
fix: correct sign error in Einstein equation
docs: add holographic principle explanation
test: verify unitarity in black hole evolution
paper: complete section on gauge emergence
```

## Integration Patterns

### Cross-Language Integration
- JSON for structured data exchange
- Plain text for simple numerical data
- Documented data formats in README

### External Tool Integration
- Command-line interfaces for all tools
- Clear input/output specifications
- Error handling and validation

## Future Structure Considerations
- Separate directories for different physics approaches
- Standardized benchmark suite location
- Machine learning experiments directory
- Interactive visualization components
- Cloud computation configurations