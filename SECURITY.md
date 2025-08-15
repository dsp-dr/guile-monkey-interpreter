# Security Policy

## ⚠️ Educational Project - Not for Production Use

**This interpreter is an educational implementation based on the book "Writing An Interpreter in Go" by Thorsten Ball.**

### Important Security Notice

This project:
- **IS** a learning tool for understanding interpreter implementation
- **IS** suitable for educational and experimental use
- **IS NOT** security-audited or hardened
- **IS NOT** intended for production environments
- **IS NOT** suitable for executing untrusted code
- **SHOULD NOT** be exposed to the internet or used in any security-sensitive context

### Known Limitations

1. **No sandboxing** - Executed code has full access to the Guile runtime
2. **No resource limits** - No protection against infinite loops or memory exhaustion
3. **No input validation** - Minimal validation of input programs
4. **FFI extensions** - When enabled, provide direct system access
5. **No security boundaries** - No isolation between interpreter and host system

### Intended Use Cases

✅ **Appropriate uses:**
- Learning about interpreters and compilers
- Understanding the Monkey language from the book
- Experimenting with language features
- Educational demonstrations
- Personal projects in trusted environments

❌ **Inappropriate uses:**
- Running untrusted user code
- Web-based code execution services
- Production applications
- Any security-sensitive context
- Processing untrusted input

### Reporting Issues

Since this is an educational project, security issues should be treated as learning opportunities:

1. **For implementation bugs**: Open a regular issue describing the problem
2. **For educational discussion**: Use issues to discuss security concepts in interpreter design
3. **For improvements**: Submit PRs that enhance understanding of secure interpreter design

### Educational Resources

If you're interested in production-ready interpreters with security considerations:

- Research sandboxing techniques (capabilities, syscall filtering)
- Study WebAssembly's security model
- Look into language-based security
- Explore memory-safe language implementations
- Review production interpreters like V8, SpiderMonkey, or LuaJIT

### Disclaimer

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY ARISING FROM THE USE OF THIS SOFTWARE.

This project is for educational purposes only. Users assume all risks associated with using this software.

### Learning Security

This project can be used to learn about security considerations in interpreter design:

- How can interpreters be made secure?
- What are the attack vectors in language runtimes?
- How do production systems handle untrusted code?
- What are the trade-offs between features and security?

These are valuable questions to explore, but remember: **this interpreter is a learning tool, not a secure system**.

---

*Based on "Writing An Interpreter in Go" - an excellent book for learning, not a guide for production systems.*