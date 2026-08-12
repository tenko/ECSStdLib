; Extra Linux AMD64 system calls

#define system_call
	.code #0
		mov	eax, #1
		mov	rdi, [rsp + 8]
		mov	rsi, [rsp + 16]
		syscall
		ret
#enddef

	system_call	sys_stat, 4, 2
	system_call	sys_ioctl, 16, 3
	system_call	sys_pipe, 22, 1
  system_call	sys_dup2, 33, 2
	system_call	sys_fork, 57, 0
	system_call sys_wait4, 61, 4
	system_call sys_kill, 62, 2
	system_call sys_execve, 59, 3
#undef system_call
