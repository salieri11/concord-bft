package profiles;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;


public interface UserRepository extends JpaRepository<User, Long> {

    List<User> findUsersByConsortiumAndOrganization(Consortium c,
                                                    Organization o);
}
