# Shape DSL Visualization Server

A sophisticated web server implementing a Domain-Specific Language (DSL) for geometric shape composition and visualization, built with Haskell and modern web technologies.

## Overview

This project demonstrates the implementation of a custom Domain-Specific Language for geometric shape manipulation, composition, and visualization. The DSL allows for complex shape compositions through a declarative syntax, with the results viewable through a web interface.

## Further Reading
For a more in-depth understanding of the DSL and its results, see the accompanying report.pdf or the results.mhtml file.

## Technical Skills Demonstrated

### Domain-Specific Language Design
- Creation of a custom DSL for geometric shape manipulation
- Implementation of compositional operations (Over, LeftOf, RightOf, Above, Below)
- Type-safe design using Haskell's strong type system
- Algebraic data types for shape representation

### Advanced Functional Programming
- Pure functional implementation in Haskell
- Advanced type system usage
- Monadic computations for IO operations
- Function composition and higher-order functions

### Computer Graphics & Geometry
- Vector and matrix transformations
- Geometric primitives implementation
- Color interpolation and gradient handling
- Bounding box calculations
- Affine transformations (rotation, scaling, shearing)

### Web Technologies
- Web server implementation using Scotty
- HTML5 template generation using Blaze
- Responsive design with CSS Grid and Flexbox
- Concurrent image rendering
- RESTful API endpoints for image serving

### Software Engineering Practices
- Modular code organization
- Clear separation of concerns
- Extensive type safety
- Parallel processing implementation
- Error handling and edge cases

## Architecture

The project is structured into several key components:

### 1. Shape DSL Core (Shape.hs)
- Fundamental geometric primitives
- Transform operations
- Color handling
- Composition operators

### 2. Rendering Engine (Render.hs)
- Vector graphics rendering
- Color interpolation
- Window management
- Image generation

### 3. Web Server (Main.hs)
- HTTP server implementation
- Template rendering
- Concurrent image processing
- Static file serving

## Key Features

- Declarative shape composition
- Rich set of geometric primitives (circles, rectangles, polygons)
- Affine transformations (scale, rotate, shear)
- Color gradients and solid colors
- Concurrent rendering
- Web-based visualization
- Responsive layout

## Required Haskell Libraries (Dependencies)
- JuicyPixels (Codec.Picture) for image processing
- Scotty for web server functionality
- Text.Blaze for HTML templating
- Data.Text.Lazy for text handling
- Control.Parallel for parallel processing
- Control.Concurrent.Async for concurrent operations
- Data.Map for efficient data structures

## Setup Instructions

# Install dependencies
stack setup

# Build the project
stack build

# Run the server
stack run

# Access the web interface
open http://localhost:3000


## License

This project is licensed under the MIT License - see the LICENSE file for details.