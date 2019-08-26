case class Undergrad()
case class Postgrad()
case class StudentId[A](id: String)

def makeStudentId(isUndergrad: Boolean): Either[StudentId[Undergrad], StudentId[Postgrad]] = {
  if (isUndergrad) {
    Left(StudentId("u1"))
  } else {
    Right(StudentId("p2"))
  }
}


def enrollInCOMP3141(id: StudentId[Undergrad]): Unit = {
}

makeStudentId(isUndergrad = false).map(enrollInCOMP3141)
makeStudentId(isUndergrad = true).map(enrollInCOMP3141)

