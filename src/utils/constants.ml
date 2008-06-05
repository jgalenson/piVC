let main_server_config_file_path = "../conf/pivc-server.conf"
let dp_server_config_file_path = "../conf/pivc-dp-server.conf"
let inductive_core_message = 
"There are some invalid VCs. However, the inductive core of the VCs is
sufficient to prove all function post-conditions, asserts (if used) and the
proof of termination (if used).

In other words, some of your conjuncts are correct, but others are incorrect
(i.e. invalid or not inductive relative to the other annotations). Your correct
conjuncts can be used alone to form a proof.

The 'Verification Condition' pane shows the correctness (inductiveness) of
each conjunct. The inductive conjuncts are marked in bold, and the non-
inductive conjuncts are marked in italics. You should remove all non-inductive
conjuncts. After you have done this, your proof will probably be complete."
let unknown_message =
"Some of your VCs do are not in the array property fragment, and hence have
unknown validity. You will need to modify your annotations to ensure that
the resulting VCs are in the array property fragment. See chapter 11.2.1
of \"The Calculus Of Computation\" for more information about the array
property fragment."
