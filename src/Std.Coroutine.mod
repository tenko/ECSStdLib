(**
Coroutine module based on Eigen Compiler Suite version
modified for static allocation of the coroutine stack
and add task oriented abstraction.

Copyright (C) Florian Negele
License is GNU General Public License version 3 with ECS Runtime Support Exception.
*)
MODULE Coroutine(StackSize, TimeType) IN Std;

IMPORT SYSTEM;

TYPE
    Coroutine* = RECORD
        frame, stack: SYSTEM.PTR;
        data : ARRAY StackSize OF SYSTEM.BYTE;
    END;
    
    Caller = RECORD (Coroutine) END;
    
    Task* = RECORD (Coroutine)
        caller*: Caller;
        time* : TimeType;
        status*: BOOLEAN;
    END;
    
    TaskEntry* = RECORD
        time* : TimeType;
        task*: POINTER TO VAR Task;
    END;

PROCEDURE (VAR caller: Caller) Call*;
BEGIN IGNORE (caller);
END Call;

PROCEDURE (VAR coroutine: Coroutine) Call*;
BEGIN END Call;

PROCEDURE- (VAR coroutine: Coroutine) Transfer* (VAR target: Coroutine);
CONST StkSize = StackSize;
VAR pointer: SYSTEM.PTR;
BEGIN
	SYSTEM.CODE ("mov ptr [$fp + pointer], ptr $fp");
	coroutine.frame := pointer;
	IF target.frame = NIL THEN
	    target.stack := PTR(target.data);
    	pointer := target.stack;
    	SYSTEM.CODE ("add ptr $sp, ptr [$fp + pointer], ptr StkSize - stackdisp");
    	target.Call;
    	SYSTEM.CODE ("mov ptr $sp, ptr $fp + pointer - stackdisp");
    	target.frame := NIL;
	ELSE
		pointer := target.frame;
		SYSTEM.CODE ("mov ptr $fp, ptr [$fp + pointer]");
	END;
END Transfer;

(** Waits for a call to the task to return and return status. *)
PROCEDURE (VAR task: Task) Await* (): BOOLEAN;
BEGIN
    task.caller.Transfer (task);
    RETURN task.status;
END Await;

(** Returns from task with request to reschedule execution. *)
PROCEDURE (VAR task: Task) Sleep*(time : TimeType);
BEGIN
    task.time := time;
    task.status := TRUE;
    task.Transfer(task.caller);
END Sleep;

(** Return from task and request to stop the task. *)
PROCEDURE (VAR task: Task) Finish*;
BEGIN
    task.time := 0;
    task.status := FALSE;
    task.Transfer(task.caller);
END Finish;

END Coroutine.
