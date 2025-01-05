# Spinal HDL Practice

Usage:
* Compile assembly and simulate on CPU: `mill t.runMain riscv.Simulate tests/test_beq.s`
* Interpret assembly: `mill t.runMain riscv.Interpret tests/test_beq.s`

The `a0` register can be initialized using an additional command line argument: `mill t.runMain riscv.Simulate tests/test_add.s 20`.
