with Ada.Real_Time;
with Ada.Text_IO;

with AWS.Default;
with AWS.Server;

with Router_CB;

procedure Main
is
   WS : AWS.Server.HTTP;
begin
   Router_Cb.Initialize_Fractals;


   Ada.Text_IO.Put_Line
     ("Serving on 127.0.0.1:" & Positive'Image (AWS.Default.Server_Port));

   AWS.Server.Start (WS,
                     "Hello World",
                     Max_Connection => 1,
                     Callback       => Router_Cb.Router'Access);

   while Router_Cb.Server_Alive loop
      delay until Ada.Real_Time.Time_Last;
   end loop;

   AWS.Server.Shutdown (WS);
end Main;
