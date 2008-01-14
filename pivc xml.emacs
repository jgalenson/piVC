Note:
	Remember to escape various things when sending from server to client
	http://hdf.ncsa.uiuc.edu/HDF5/XML/xml_escape_chars.htm (both ways?)


Connect:

Client: <piVC_transmission type="connect_request" />
Server:
<piVC_transmission type="connect_response" >
	<client_id> id_num </client_id>
</piVC_transmission>

Disconnect:

Client: <piVC_transmission type="disconnect_request" />
Server: <piVC_transmission type="disconnect_response" />

Program send:

Client:
<piVC_transmission type="program_submission_request" >
	<client_id> id_num </client_id>
	<code> code </code>
</piVC_transmission>

Server errors:
<piVC_transmission type="program_submission_response" >
	<result status="error">
		<error type="semantic_error/syntax_error">
			<location>
				<start row=foo col=bar />
				<end row=bar col=foo />
			</location>
			<message>
				Error!
			</message>
		</error>
		// more errors as siblings
	</result>
</piVC_transmission>

Server no errors:
<piVC_transmission type="program_submission_response" >
	<result status="proved/unproved">
		<basic_path status="valid/invalid">
			<path>
				<node type="expr/assume/annotation">
					<location>
						<start row=foo col=bar />
						<end row=bar col=foo />
					</location>
					<text>
						cool++;
					</text>
				</node>
				// mode nodes as siblings
			</path>
			<vc>
				string goes here
			</vc>
		</basic_path>
		// mode basic paths as siblings
	</result>
</piVC_transmission>