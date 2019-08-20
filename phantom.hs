data Undergrad -- empty type
data Postgrad
data StudentID s = SID String

mkStudentID :: Bool -> Either (StudentID Undergrad) (StudentID Postgrad)
mkStudentID isUndergrad = 
  if isUndergrad 
    then Left (SID "u1") 
    else Right (SID "p2")

enrolInCOMP3141 :: StudentID Undergrad -> IO ()
enrolInCOMP3141 (SID xs) = 
  print ("Enrolled " ++ xs ++ " in COMP3141")

lookupTranscript :: StudentID x -> IO ()
lookupTranscript (SID xs) = 
  print "Printing transcript of " ++ xs

main = do
  let isUndergrad = False
  let sid :: Either (StudentID Undergrad) (StudentID Postgrad) = mkStudentID isUndergrad
  traverse enrolInCOMP3141 sid 
  -- traverse will `fmap` over the Right projection 
  -- and then `sequence`