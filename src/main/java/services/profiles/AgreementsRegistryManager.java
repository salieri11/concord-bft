/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package services.profiles;

import java.util.Optional;

import javax.transaction.Transactional;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * This class manages all persistence related operations related to Agreement
 * management API.
 */
@Component
@Transactional
public class AgreementsRegistryManager {

   @Autowired
   private AgreementRepository agreementRepository;

   /** Needed for spring */
   protected AgreementsRegistryManager() {
   }

   public JSONObject getAgreementWithID(String id) {
      JSONObject json = new JSONObject();
      Optional<Agreement> oAgreement = agreementRepository.findById(Long.parseLong(id));

      if (oAgreement.isPresent()) {
         Agreement a = oAgreement.get();
         Boolean accepted = a.getAcceptance();
         json.put("id", a.getID());
         json.put("type", a.getType());
         json.put("accepted", accepted);

         // Don't send legal agreement if already accepted
         // it's too heavy
         if (accepted == Boolean.FALSE) {
            json.put("content", a.getContent());
         }
      }

      return json;
   }

   public JSONObject updateAgreement(String id, JSONObject request) {
      JSONObject json = new JSONObject();
      Optional<Agreement> oAgreement = agreementRepository.findById(
                                       Long.parseLong(id));

      if (oAgreement.isPresent()) {
         Agreement agreement = oAgreement.get();
         if (request.get("accepted") == Boolean.TRUE) {
            agreement.accepted();
            agreement.setFirstName(request.get("first_name").toString());
            agreement.setLastName(request.get("last_name").toString());
            agreement.setCompany(request.get("company").toString());
            agreement.setAcceptedOn();
         }
         agreementRepository.save(agreement);
      }

      return json;
   }
}
