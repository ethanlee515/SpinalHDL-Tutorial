# Spinal HDL Practice

* Compile assembly and simulate on CPU: `mill t.runMain riscv.Simulate tests/test_beq.s`
* Interpret assembly: `mill t.runMain riscv.Interpret tests/test_beq.s`

You can also initialize the `a0` register using an additional command line argument: `mill t.runMain riscv.Simulate tests/test_add.s 20`.
