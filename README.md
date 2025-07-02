# Somsed

A **high-performance compiler and client** for Desmos(written in rust).

# Planned features
- [x] basic statments, function calls and variables
- [x] native compilation
- [x] list types
- [x] wack scopes(missing tests) 
- [ ] not leaking memory with list types
- [ ] more advanced functions and operators
- [ ] conditionals
- [x] for statments
- [ ] control flow optimization
- [ ] close to full desmos compatibilty
- [ ] gpu compilation
## Usage  

1. Start the client:  
   ```bash
   cargo run
   ```
the client looks and functions quite simular to desmos, but expressions are parsed in their raw latex form.

![image](https://github.com/user-attachments/assets/954b621d-deef-4d29-97ef-2b5ce4a380bb)

## Contributing
At the moment documentation is really bad for such a complex project.
If anyone is intrested in contributing i suggest you message me privately on discord(@catted.) or on gitter(@urisinger_gitlab:gitter.im) and i would explain everything you need to know about how the project works. 
There are currently alot of bugs and crashes, so reporting them and addings tests for them could help me alot.
